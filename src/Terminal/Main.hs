{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (fromRight)
import Data.List (intercalate)
import Data.Loc
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Version
import Language.Lexer.Applicative (streamToList)
import Parse.Lexer
import Parse.Topoi
import qualified Paths_topoi as TOPOI
import Syntax.Concrete
import System.Environment (getArgs)

-- import Type.Infer

strip :: String -> String
strip str = Text.unpack . Text.strip $ Text.pack str

-- can use https://github.com/pcapriotti/optparse-applicative
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
  -- print . streamToList $ fromRight (error "token issue") tokens
  print tokens

-- putStrLn "-- Parse Tree --"
-- let csTree = parse typeSignature pathName (fromRight undefined tokens)
-- print csTree

-- putStrLn "-- Type Check --"
-- let (Program exprs _) = fromRight (Program [] NoLoc) csTree
-- print . runSubstituteM . inferProgram $ exprs

helpMsg :: String
helpMsg =
  "Topoi compiler "
    <> intercalate "." (show <$> versionBranch TOPOI.version)
    <> "\n"
    <> "Usage: \n"
    <> "  topoi -c src      compile and print information\n"
    <> "  topoi help        show this help message\n"
