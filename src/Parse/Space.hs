{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Parse.Space
  ( Parser,
  )
where

import Data.Word (Word8)
import qualified Parse.Primitives as P
import Parse.Primitives (Col, Row)
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E

type Parser x a = P.Parser x (a, A.Position)
