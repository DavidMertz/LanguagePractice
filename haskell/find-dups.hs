module Main (main) where

import Crypto.Hash 
import Control.Monad (forM_)
import System.Directory.PathWalk (pathWalk)
import System.FilePath (joinPath)
import System.Environment (getArgs)

processPath :: String -> IO ()
processPath = putStrLn

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = if rawArgs == [] then ["."] else rawArgs
  forM_ args $ \arg -> do
    pathWalk arg $ \root dirs files -> do
      forM_ files (\file -> processPath (joinPath [root, file]))
