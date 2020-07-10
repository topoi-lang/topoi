module Main where

import System.Environment (getArgs)
import qualified Paths_topoi as TOPOI
import Data.Version
import Data.List (intercalate)

import Topoi.Compiler (print'')

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cmd, path] | cmd == "-c" -> mconcat [putStr path, print'']
    otherwise -> putStrLn helpMsg

helpMsg :: String
helpMsg =
  "Topoi compiler "
    <> intercalate "." (show <$> versionBranch TOPOI.version)
    <> "\n"
    <> "Usage: \n"
    <> "  topoi -c src      compile and print information\n"
    <> "  topoi help        show this help message\n"