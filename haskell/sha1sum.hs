import Crypto.Hash 
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as Bytes

main :: IO ()
main = do
  [file] <- getArgs
  content <- Bytes.readFile file
  putStrLn $ show (hashWith SHA1 content) ++ "  " ++ file

