module Type.Error where

import Data.Loc
import Data.Text (Text)
import Syntax.Abstract

data TypeError
  = Failed Text Loc
  | UnifyFailed Type Type Loc
  | NotAFunction Type Loc
  | ReassignConst Text Loc
  deriving (Show)
