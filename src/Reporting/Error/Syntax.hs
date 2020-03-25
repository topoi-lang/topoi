module Reporting.Error.Syntax where

import Parse.Primitives (Col, Row)
import Prelude hiding (String)

-- Should add some more unaccepted space char here
data Space
  = OpenCommentBlock
  | UnknownChar

data SExpr
  = Start Row Col

data Atom
  = AtomUnknownChar
  | AtomEndless

{- This is for S expression -}
data Parens = ParensOpen
