-- Thanks to https://stackoverflow.com/users/7203016/k-a-buhr
-- for providing guidance on a much better way to structure this tool
import System.Directory (getFileSize, getHomeDirectory)
import System.Directory.PathWalk (pathWalkLazy)
import System.Environment (getArgs)
import System.FilePath ((</>), addTrailingPathSeparator, normalise)
import System.Path.NameManip (guess_dotdot, absolute_path)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Foldable (for_)
import Data.Typeable (typeOf)
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.Map.Strict as Map
import Control.Monad (mapM_, forM_)
import Crypto.Hash -- Implicit, cannot list SHA1 here?!

-- We are building in assumption of SHA1 digest length
data Sha1 = Sha1 String deriving (Show, Eq)

-- "getter" and "setter" for hashes
sha1FromString :: String -> Maybe Sha1
sha1FromString s | 40 /= length s = Nothing
                 | otherwise     = Just (Sha1 s)

stringFromSha1 :: Sha1 -> String
stringFromSha1 (Sha1 s) = s

-- File-info record
data Finfo = Finfo { 
    fname :: String, 
    f_size :: Integer,   -- size of file
    sha1 :: Digest SHA1 } deriving (Show, Eq)

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

-- Create the BySize map
getBySize :: FilePath -> IO BySize
getBySize root = do
  -- first, get all the files
  files <- getAllFiles root
  -- convert them all to finfos
  finfos <- mapM getFinfo files
  -- get a list of size/finfo pairs
  let pairs = [(f_size finfo, finfo) | finfo <- finfos]
  -- convert it to a map, allowing duplicate keys
  return $ fromListWithDuplicates pairs

groupByHash :: [Finfo] -> ByHash
groupByHash finfos = 
    let bySha1 = [(sha1 finfo, finfo) | finfo <- finfos] 
    in fromListWithDuplicates bySha1

-- this is a little complicated, but standard
fromListWithDuplicates :: Ord k => [(k, v)] -> Map k [v]
fromListWithDuplicates pairs = 
    Map.fromListWith (++) [(k, [v]) | (k, v) <- pairs]

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles root = do
  nodes <- pathWalkLazy root
  -- get file paths from each node
  let files = [dir </> file | (dir, _, files) <- nodes, file <- files]
  return files

-- The logic of processing files    
getFinfo :: FilePath -> IO Finfo
getFinfo path = do
    abspath <- do absolutize path
    size <- do getFileSize abspath
    content <- Bytes.readFile abspath
    let sha1 = hashWith SHA1 content
    let finfo = Finfo abspath size sha1
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

-- The "main" function walks directory with callback  
main :: IO ()
main = do
    rawArgs <- getArgs
    let rootdir = if rawArgs == [] then "." else head rawArgs
    bySize <- getBySize rootdir
    let sizeRecords = reverse $ Map.assocs bySize
    mapM_ printSizeRecord sizeRecords

