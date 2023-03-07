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
import System.IO (hPutStrLn, stderr)    
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Foldable (for_)
import Data.Typeable (typeOf)
import qualified Data.ByteString.Char8 as Bytes
import Data.ByteString (empty)
import Data.ByteString.Char8 (singleton, pack)
import Control.Monad (mapM, mapM_, forM, forM_, filterM, liftM)
import Control.Monad.IO.Class (liftIO)
import Util (lengthExceeds)
import Crypto.Hash -- Implicit, cannot list SHA1 here?!

-- Several mappings used by functions
type BySize = Map Integer [Finfo]
type ByHash = Map (Digest SHA1) [Finfo]
type ByInode = Map (CIno) [Finfo]

-- File-info record
data Finfo = Finfo { 
    fname :: String, 
    f_size :: Integer,   -- size of file
    inode :: CIno        -- inode on disk 
    } deriving (Show, Eq)

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

-- Create the ByInode map on same-sized files
groupByInode :: [Finfo] -> ByInode
groupByInode finfos = 
    let byInode = [(inode finfo, finfo) | finfo <- finfos] 
    in fromListWithDuplicates byInode

-- Lazily return all files from rootdir
getAllFilesRaw :: FilePath -> IO [FilePath]
getAllFilesRaw root = do
  nodes <- pathWalkLazy root
  -- get file paths from each node
  let files = [dir </> file | (dir, _, files) <- nodes,
                              file <- files]
  return files

-- Lazily return (normal) files from rootdir
getAllFilesNoSymFiles :: FilePath -> IO [FilePath]
getAllFilesNoSymFiles root = do
    nodes <- pathWalkLazy root
    let files = [dir </> file | (dir, _, files) <- nodes,
                                file <- files]
    normalFiles <- filterM (liftM not . pathIsSymbolicLink) files
    return normalFiles

-- This version based on StackOverflow by ChrisB at:
-- stackoverflow.com/questions/68869527/getallfiles-but-not-symlinks/
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
                    let descend = getAllFilesNoSymLinks . (path </>)
                        in fmap concat $ mapM descend dirContents

-- Cache the read and compute on the inode
getHash :: String -> IO (Digest SHA1)
getHash abspath = do
    content <- Bytes.readFile abspath
    let sha1 = hashWith SHA1 content
    return sha1

-- The logic of processing files    
getFinfo :: FilePath -> IO Finfo
getFinfo path = do
    fstat <- getFileStatus path
    let inode = fileID fstat
    abspath <- absolutize path
    size <- getFileSize abspath
    let finfo = Finfo abspath size inode
    return finfo

-- Identify all patterns of multiple fnames with same hash
getHashDups :: [((Digest SHA1), [Int])] -> [[Finfo]] -> [((Digest SHA1), [String])]
getHashDups hashToIndices inodeFinfos = 
    let hashToFname = [(sha1, fname finfo) | 
                       (sha1, indices) <- hashToIndices,
                       index <- indices,
                       finfo <- (inodeFinfos !! index) ]
    -- Point-free is O(N) on size:  ((> 1) . length . snd) 
    in filter (flip lengthExceeds 1 . snd)
        (Map.assocs $ fromListWithDuplicates hashToFname)

-- Break out report logic for multiple inodes of same size
printSizeMultiInodes :: Integer -> ByInode ->  [Digest SHA1] -> IO ()
printSizeMultiInodes size byInode sha1s = do
    -- Use list of fnames by inode to allow position indexing
    let inodeFinfos = Map.elems byInode
    let sha1sIndices = zip sha1s [0..]
    let hashToIndices = Map.assocs $ fromListWithDuplicates sha1sIndices
    let hashDups = getHashDups hashToIndices inodeFinfos
    -- hashDups may be empty, but if multiple, report fnames
    forM_ hashDups (\(sha1, fnames) -> do
        if length fnames > 1 then do
            putStrLn $ "Size: " ++ show size ++ " | SHA1: " ++ show sha1
            forM_ fnames (\fname -> do
                putStrLn $ "  " ++ fname ) 
        else do return () ) 

-- Print report on specific file size (if any dups) 
printSizeRecord :: (Integer, [Finfo]) -> IO ()
printSizeRecord (size, finfos)
    | size > 0 && length finfos > 1 = do
      let byInode = groupByInode finfos 
      -- There are multiple filenames of `size` but just one inode
      if length byInode == 1 then do
          let ino = inode $ head finfos
          putStrLn $ "Size: " ++ show size ++ " | SHA1: <INODE " ++ show ino ++ ">"
          for_ [fname finfo | finfo <- finfos] (\fname -> do
              putStrLn $ "  " ++ fname )
      else do
          let byInode = groupByInode finfos
          -- `sha1s` is just a list of hashes; need to match with byInode
          -- Each one pertains to one more filenames with same inode
          hashes <- forM (Map.assocs byInode) (\(inode, finfos) -> do
              return (getHash (fname $ head finfos)) )
          sha1s <- forM hashes (\hash -> liftIO hash)
          -- Report iff there are duplicate hashes among these
          printSizeMultiInodes size byInode sha1s
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

