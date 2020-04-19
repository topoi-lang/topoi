module Reporting.Error where

import Parse.Lexer (LexicalError)
import Parse.SExprParser (SyntacticError)

-- Errors from different phrases / passes
data Error
  = LexicalError LexicalError
  | SyntacticError SyntacticError
  deriving (Show)
