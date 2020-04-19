{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (fromRight)
import Data.List (intercalate)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Version
import Language.Lexer.Applicative (streamToList)
import Parse.Lexer
import Parse.SExprParser
import qualified Paths_topoi as TOPOI
import System.Environment (getArgs)

strip :: String -> String
strip str = Text.unpack . Text.strip $ Text.pack str

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cmd, path] | (strip cmd) == "-c" -> compile path
    _ -> putStr helpMsg

compile :: String -> IO ()
compile pathName = do
  sourceFile <- Text.readFile pathName
  putStrLn "-- Tokens --"
  let tokens = scan pathName sourceFile
  print . streamToList $ fromRight (error "token issue") tokens
  putStrLn "-- Parse Tree --"
  let csTree = parse expressionList pathName (fromRight undefined tokens)
  print csTree

helpMsg :: String
helpMsg =
  "Topoi compiler "
    <> intercalate "." (show <$> versionBranch TOPOI.version)
    <> "\n"
    <> "Usage: \n"
    <> "  topoi -c src      compile and print information\n"
    <> "  topoi help        show this help message\n"
