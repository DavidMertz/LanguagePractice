{-# LANGUAGE BangPatterns #-}

-- Thanks to https://stackoverflow.com/users/7203016/k-a-buhr
-- for providing guidance on a much better way to structure this tool
import System.Directory (getFileSize, getHomeDirectory, 
                         pathIsSymbolicLink, doesFileExist,
                         listDirectory)
import System.Directory.PathWalk (pathWalkLazy)
import System.Environment (getArgs)
import System.FilePath ((</>), addTrailingPathSeparator, normalise)
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.Posix.Files (getFileStatus, fileID)
import System.Posix.Types (CIno)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Foldable (for_)
import Data.Typeable (typeOf)
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.Map.Strict as Map
import Control.Monad (mapM_, forM_, filterM, liftM)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash -- Implicit, cannot list SHA1 here?!

-- File-info record
data Finfo = Finfo { 
    fname :: String, 
    f_size :: Integer,   -- size of file
    sha1 :: Digest SHA1, -- fingerprint
    inode :: CIno        -- inode on disk 
    } deriving (Show, Eq)

type BySize = Map Integer [Finfo]
type ByHash = Map (Digest SHA1) [Finfo]

-- Recipe from https://www.schoolofhaskell.com
absolutize :: String -> IO String
absolutize aPath 
  | "~" `isPrefixOf` aPath = do
    homePath <- getHomeDirectory
    return $ normalise $ addTrailingPathSeparator homePath 
                           ++ tail aPath
  | otherwise = do
    pathMaybeWithDots <- absolute_path aPath
    return $ fromJust $ guess_dotdot pathMaybeWithDots

-- Create Map from list of pairs, making dup vals into a list
fromListWithDuplicates :: Ord k => [(k, v)] -> Map k [v]
fromListWithDuplicates pairs = 
    Map.fromListWith (++) [(k, [v]) | (k, v) <- pairs]

-- Create Map from list of pairs, discarding vals with dup key
fromListWithFirst :: Ord k => [(k, v)] -> Map k v
fromListWithFirst pairs = 
    Map.fromListWith (\a _ -> a) [(k, v) | (k, v) <- pairs]

-- Create the ByHash map on same-sized files
groupByHash :: [Finfo] -> ByHash
groupByHash finfos = 
    let bySha1 = [(sha1 finfo, finfo) | finfo <- finfos] 
    in fromListWithDuplicates bySha1

-- Lazily return (normal) files from rootdir
getAllFilesNoSymFiles :: FilePath -> IO [FilePath]
getAllFilesNoSymFiles root = do
    nodes <- pathWalkLazy root
    -- get file paths from each node
    let files = [dir </> file | (dir, _, files) <- nodes,
                                file <- files]
    normalFiles <- filterM (liftM not . pathIsSymbolicLink) files
    return normalFiles

-- Lazily return all files from rootdir
getAllFilesRaw :: FilePath -> IO [FilePath]
getAllFilesRaw root = do
  nodes <- pathWalkLazy root
  -- get file paths from each node
  let files = [dir </> file | (dir, _, files) <- nodes,
                              file <- files]
  return files

-- Lazily return (normal) files from (normal) rootdir
getAllFilesNoSymLinks :: FilePath -> IO [FilePath]
getAllFilesNoSymLinks path = do
    isSymlink <- pathIsSymbolicLink path
    if isSymlink 
        -- if this is a symlink, return the empty list. 
        then return [] 
        else do
            isFile <- doesFileExist path
            if isFile 
                then return [path] 
                else do
                    -- if not a file, assume it to be a directory
                    dirContents <- listDirectory path
                    -- run recursively on children and accumulate results
                    fmap concat $ mapM (getAllFiles . (path </>)) dirContents

-- The logic of processing files    
getFinfo :: FilePath -> IO Finfo
getFinfo path = do
    abspath <- absolutize path
    size <- getFileSize abspath
    content <- Bytes.readFile abspath
    fstat <- getFileStatus path
    let inode = fileID fstat
    let sha1 = hashWith SHA1 content
    let finfo = Finfo abspath size sha1 inode
    return finfo

-- Pring report on individual BySize record (if qualifying)
printSizeRecord :: (Integer, [Finfo]) -> IO ()
printSizeRecord (size, finfos)
    | size > 0 && length finfos > 1 = do
      let byHash = groupByHash finfos
      for_ (Map.assocs byHash) (\(sha1, finfos) -> do
        if (length finfos) > 1 then do
          putStrLn $ "Size: " ++ show size ++ " | SHA1: " ++ show sha1
          for_ [fname finfo | finfo <- finfos] (\fname -> do
                putStrLn $ "  " ++ fname )
        else do
          return () )
    | otherwise = do
      return ()

-- Create the BySize map
getBySize :: FilePath -> IO BySize
getBySize root = do
    -- first, get all the files
    files <- getAllFilesNoSymLinks root
    -- convert them all to finfos
    finfos <- mapM getFinfo files
    -- get a list of size/finfo pairs
    let pairs = [(f_size finfo, finfo) | finfo <- finfos]
    -- convert it to a map, allowing duplicate keys
    return $ fromListWithDuplicates pairs

-- The "main" function walks directory with callback  
main :: IO ()
main = do
    rawArgs <- getArgs
    let rootdir = if rawArgs == [] then "." else head rawArgs
    bySize <- getBySize rootdir
    let sizeRecords = reverse $ Map.assocs bySize
    mapM_ printSizeRecord sizeRecords

    -- XXX: debugging
--    files <- getAllFiles rootdir
--    putStrLn ("getAllFiles finds:     " ++ show (length files))
--    files' <- getAllFilesNoSymFiles rootdir
--    putStrLn ("getAllFilesNoSymFiles: " ++ show (length files'))
--    files'' <- getAllFilesNoSymDirs rootdir
--    putStrLn ("getAllFilesNoSymDirs:  " ++ show (length files''))

