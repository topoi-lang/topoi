{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Parse.Space where

import Data.Word (Word8)
import Parse.Internal (Col, Row)
import qualified Parse.Internal as I
import qualified Reporting.Error.Syntax as E

chomp :: (E.Space -> Row -> Col -> x) -> I.Parser x ()
chomp toError =
  P.Parser $ \(P.State src offset indent row col) consumeOk _ consumeErr _ ->
    let (# status, newPos, newRow, newCol #) = eatSpaces pos end row col
     in undefined

-- Should define the status and error here
data Status = String

eatSpaces :: Word8 -> Word8 -> Row -> Col -> (# Status, Word8, Row, Col #)
eatSpaces = undefined
