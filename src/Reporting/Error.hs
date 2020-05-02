module Reporting.Error where

import Parse.Lexer (LexicalError)
import Parse.SExprParser (SyntacticError)
import Type.Error (TypeError)

-- Errors from different phrases / passes
data Error
  = LexicalError LexicalError
  | SyntacticError SyntacticError
  | UnificationError TypeError
  deriving (Show)
