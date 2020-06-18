module Main where

import System.Environment (getArgs)
import qualified Paths_topoi as TOPOI
import Data.Version
import Data.List (intercalate)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [cmd, path] | cmd == "-c" -> putStrLn path
        _ -> putStrLn helpMsg

helpMsg :: String
helpMsg =
  "Topoi compiler "
    <> intercalate "." (show <$> versionBranch TOPOI.version)
    <> "\n"
    <> "Usage: \n"
    <> "  topoi -c src      compile and print information\n"
    <> "  topoi help        show this help message\n"