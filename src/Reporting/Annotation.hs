module Reporting.Annotation where

import Data.Word (Word16)

data Position
  = Position
      {-# UNPACK #-} !Word16
      {-# UNPACK #-} !Word16
