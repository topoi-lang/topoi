module Parse.SExprParser where

import Data.Loc
import Data.Void
import Parse.Lexer
import qualified Text.Megaparsec as MP
import Text.Megaparsec hiding (ParseError, Pos, State, parse)
import Prelude hiding (Ordering (..))

type Parser = ParsecT Void TokStream {- Return type-}

type SyntacticError = (Loc, String)

parse :: Parser a -> FilePath -> TokStream -> Either [SyntacticError] a
parse parser filepath tokenStream = undefined
