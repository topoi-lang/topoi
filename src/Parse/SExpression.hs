module Parse.SExpression where

import qualified AST.Source as Src
import Parse.Primitives
import qualified Parse.Space as Space
import qualified Reporting.Error.Syntax as Err

expression :: Space.Parser Err.Expr Src.Expr
expression = do
  startPos <- getPosition
  oneOf Err.Start [string start]

-- We treat atom as string
string :: A.Position -> Parser Err.Expr Src.Expr
string start = do
  str <- String.string Err.Start Err.String
  addEnd start (Src.Str str)
