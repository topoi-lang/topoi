module Parse.SExpression where

import qualified AST.Source as Src
import qualified Parse.CharacterCodes as C
import Parse.Primitives
import qualified Parse.Space as Space
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Err

expression :: Space.Parser Err.SExpr Src.SExpr
expression = do
  startPos <- getPosition
  oneOf Err.Start [string start]

parens :: A.Position -> Parser Err.SExpr Src.SExpr
parens start@(A.Position row col) =
  inContext Err.Parens (word1 C.parenLeft Err.Start) $ do
    oneOf
      Err.ParensOpen
      [ do
          (entry, end) <- specialize Err.SExpr expression
          chompParens start [entry],
        do
          word1 C.parenRight Err.ParensOpen
          addEnd start (Src.SExpr [])
      ]

chompParens :: A.Position -> [Src.SExpr] -> Parser Err.SExpr Src.SExpr
chompParens start entries =
  oneOf
    Err.ParenEnd
    [ do
        word1 CC.Space Err.ParenEnd
        (entry, end) <- specialize Err.SExpr expression
        chompParens start (entry : entries),
      do
        word1 CC.parenRight Err.ParensOpen
        addEnd start (Src.SExpr (reverse entries))
    ]
