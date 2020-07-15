{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Topoi.Compiler.Parser.Test where

import Topoi.Compiler.Parser

import GHC.Exts
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

--------------------------------------------------
-- Testing
--------------------------------------------------
data Result e a = OK a B.ByteString | Err (Error e) B.ByteString
  deriving Show

runParser :: Parser e a -> B.ByteString -> Result e a
runParser (Parser f) b = unsafeDupablePerformIO do
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let !(I# len) = B.length b
    let end = plusAddr# buf len
    case f end buf of
      Err# e s  -> do
        let offset = minusAddr# s buf
        pure (Err e (B.drop (I# offset) b))
      OK# a s   -> do
        let offset = minusAddr# s buf
        pure (OK a (B.drop (I# offset) b))

testParser :: Parser e a -> String -> Result e a
testParser pa s = runParser pa (B.pack (concatMap charToBytes s))

ws = manyTok_ ($(char ' ') <!> $(char '\n'))
open = $(char '(') >> ws
close = $(char ')') >> ws
ident = someTok_ (satisfyA isLatinLetter) >> ws
sexp = br open (some_ sexp >> close) ident
src = sexp >> eof
runSexp = runParser src