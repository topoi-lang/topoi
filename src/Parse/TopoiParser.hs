module Parse.TopoiParser where

import Control.Monad (void)
import Data.Loc
import Data.Text (Text)
import Data.Void
import qualified Parse.Combinator as PC
import Parse.Lexer
import Reporting.Annotation (PosLog)
import qualified Reporting.Annotation as A
import qualified Text.Megaparsec as MP
import Text.Megaparsec hiding (ParseError, Pos, State, parse)

type Parser = ParsecT Void TokStream (PosLog Tok)

type SyntacticError = (Loc, String)

parse :: Parser a -> FilePath -> TokStream -> Either [SyntacticError] a
parse parser filepath tokenStream =
  case A.runPosLog (runParserT parser filepath tokenStream) of
    Left e -> Left (fromParseErrorBundle e)
    Right x -> Right x
  where
    fromParseErrorBundle ::
      ShowErrorComponent e =>
      ParseErrorBundle TokStream e ->
      [SyntacticError]
    fromParseErrorBundle (ParseErrorBundle errors posState) =
      snd $ foldr f (posState, []) errors
      where
        f ::
          ShowErrorComponent e =>
          MP.ParseError TokStream e ->
          (PosState TokStream, [SyntacticError]) ->
          (PosState TokStream, [SyntacticError])
        f err (initial, accum) =
          let (_, next) = reachOffset (errorOffset err) initial
           in (next, (getLoc err, parseErrorTextPretty err) : accum)

getLoc :: ShowErrorComponent e => MP.ParseError TokStream e -> Loc
getLoc (TrivialError _ (Just (Tokens xs)) _) = foldMap locOf xs
getLoc _ = mempty
