module Main (main) where

import Crypto.Hash 
import Control.Monad (forM_)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as Bytes
import System.Directory.PathWalk (pathWalk)
import System.FilePath (joinPath, addTrailingPathSeparator, normalise)
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.Directory (getHomeDirectory)
import System.Posix (getFileStatus, fileSize)
import System.Environment (getArgs)

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
  f_size :: Int, -- size of file
  sha1 :: String } deriving (Show, Eq)

type BySize = Map Int [Finfo]
type ByHash = Map String [Finfo]

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

-- Recipe from StackOverflow
getFileSize :: String -> IO Int
getFileSize path = do
    stat <- do getFileStatus path
    return $ fromIntegral (fileSize stat)

-- The logic of processing files    
processPath :: String -> IO ()
processPath path = do
  abspath <- do absolutize path
  size <- do getFileSize abspath
  content <- Bytes.readFile abspath
  --let sha1 = hashWith SHA1 content
  let sha1 = sha1FromString "NO HASH"
  putStrLn (abspath ++ " " ++ show size ++ " " ++ show sha1)

-- Walk directories and return two maps
walkDir :: String -> IO (BySize)
walkDir rootdir = do
  pathWalk rootdir $ \root dirs files -> do
    forM_ files (\file -> processPath (joinPath [root, file]))
  return Map.empty

-- The "main" function walks directory with callback  
main :: IO ()
main = do
  rawArgs <- getArgs
  let rootdir = if rawArgs == [] then "." else head rawArgs
  result <- walkDir rootdir

  putStrLn (show result)
