module Core.Parser where

import Core.Parser.RawAST (Parser, SExpr(..))

sc :: Parser ()
sc = space space1 lineComment empty

symbol :: Parser Text
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"
