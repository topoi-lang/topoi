module Reporting.Annotation where

{- TODO: should remove this Position and importing SrcLoc haskell package -}
import Data.Word (Word16)

-- | Position type
data Position
  = Position
      {-# UNPACK #-} !Word16
      {-# UNPACK #-} !Word16

data Region
  = Region
      {-# UNPACK #-} !Position
      {-# UNPACK #-} !Position

data Located a = At Region a

at :: Position -> Position -> a -> Located a
at start end value = At (Region start end) value
