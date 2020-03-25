{-# LANGUAGE BangPatterns #-}

module Parse.String where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import qualified Parse.CharacterCodes as C
import qualified Parse.Primitives as P
import Parse.Primitives (Col, Parser, Row)
import qualified Reporting.Error.Syntax as Err

isDoubleQuote :: Ptr Word8 -> Ptr Word8 -> Bool
isDoubleQuote pos end =
  pos < end && P.unsafeIndex pos == C.doubleQuote

isSingleQuote :: Ptr Word8 -> Ptr Word8 -> Bool
isSingleQuote pos end =
  pos < end && P.unsafeIndex pos == C.singleQuote

data AtomResult
  = Good (Ptr Word8) Row Col !String
  | Bad Row Col Err.Atom
