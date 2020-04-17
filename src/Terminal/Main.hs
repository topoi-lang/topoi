{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (fromRight)
import qualified Data.Text.IO as Text
import Language.Lexer.Applicative (streamToList)
import Parse.Lexer
import Parse.SExprParser
import System.Environment (getArgs)

main :: IO ()
main = do
  (arg : _) <- getArgs
  sourceFile <- Text.readFile arg
  putStrLn "-- Tokens --"
  let tokens = scan arg sourceFile
  print . streamToList $ fromRight (error "token issue") tokens
  putStrLn "-- Parse Tree --"
  let csTree = parse expressionList arg (fromRight undefined tokens)
  print csTree
