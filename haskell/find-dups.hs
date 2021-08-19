{-# LANGUAGE BangPatterns #-}
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
    sha1 :: Digest SHA1 } deriving (Show, Eq)

type BySize = Map Int [Finfo]
type ByHash = Map String [Finfo]

-- Compute a new map with this file entry added
addBySize :: Int -> Finfo -> BySize -> BySize
addBySize size finfo bySize = 
    case (Map.lookup size bySize) of
        Nothing -> Map.insert size [finfo] bySize
        Just sizeEntry -> 
            Map.adjust (\_ -> (finfo : sizeEntry)) size bySize

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

-- The size of a file given its path
getFileSize :: String -> IO Int
getFileSize path = do
    stat <- do getFileStatus path
    return $ fromIntegral (fileSize stat)

-- The logic of processing files    
processPath :: String -> BySize -> IO (Finfo)
processPath path bySize = do
    !abspath <- do absolutize path
    !size <- do getFileSize abspath
    content <- Bytes.readFile abspath
    let sha1 = hashWith SHA1 content
    let finfo = Finfo abspath size sha1
    return finfo

sizeHist :: State [bySize]
sizeHist = do
    by_size <- get()
    put by_size
    get


-- Walk directories and return two maps
walkDir :: String -> BySize -> IO ([BySize])
walkDir rootdir bySize = do
    let !bySizeHist = [bySize]
    pathWalk rootdir (\root dirs files -> do
        forM_ files $ (\file -> do
            let !latest = head bySizeHist
            finfo <- do processPath (joinPath [root, file]) latest
            let !new = addBySize (f_size finfo) finfo latest

            let latest_size = Map.keys latest
            let new_size = Map.keys new
            let error = if latest == new 
                then 
                    "Error, identical maps!" 
                else 
                    "Update of map is fine" ++ (show latest_size) ++ (show new_size)
            putStrLn error

            let !bySizeHist = [new] ++ bySizeHist
            putStrLn (fname finfo) ))
    return bySizeHist

-- The "main" function walks directory with callback  
main :: IO ()
main = do
  rawArgs <- getArgs
  let rootdir = if rawArgs == [] then "." else head rawArgs
  !result <- walkDir rootdir Map.empty

  putStrLn (show result)
