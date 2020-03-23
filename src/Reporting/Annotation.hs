module Reporting.Annotation where

import Data.Word (Word16)

data Position
  = Position
      {-# UNPACK #-} !Word16
      {-# UNPACK #-} !Word16

data Located a = At Region a

data Region = Region Position Position

at :: Position -> Position -> a -> Located a
at start end value = At (Region start end) value
