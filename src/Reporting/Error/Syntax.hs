module Reporting.Error.Syntax where

import Parse.Primitives (Col, Row)

-- Should add some more unaccepted space char here
data Space
  = OpenCommentBlock
  | UnknownChar

data Expr
  = Start Row Col
  | String
