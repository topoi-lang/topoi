{-# LANGUAGE BangPatterns #-}

module Parse.String where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Parse.Primitives
import qualified Reporting.Error.Syntax as Err

string ::
  (Row -> Col -> x) ->
  (Err.String -> Row -> Col -> x) ->
  Parser x String
string = undefined

-- string toExpectation toError =
--   Parser $ \(State src pos end indent row col) cok _ cerr eerr ->

isDoubleQuote :: Ptr Word8 -> Ptr Word8 -> Bool
isDoubleQuote pos end =
  pos < end && P.unsafeIndex pos == 0x22 {- " -}
