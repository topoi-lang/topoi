{-# LANGUAGE OverloadedStrings #-}

module Parse.SExprParser where

import qualified AST.Source as Src
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
import Prelude hiding (Ordering (..))

-- This is the parser of the S-Expression

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

symbol :: Tok -> Parser ()
symbol t = do
  PC.symbol t
  void $ many (PC.ignore TokNewline)

parens :: Parser a -> Parser a
parens =
  between
    (symbol TokParenOpen <?> "left parenthesis")
    (symbol TokParenClose <?> "right parenthesis")

identifierName :: Parser Text
identifierName = PC.extract f
  where
    f (TokIdent s) = Just s
    f _ = Nothing

atomName :: Parser Text
atomName = PC.extract f
  where
    f (TokAtom s) = Just s
    f _ = Nothing

integer :: Parser Int
integer = PC.extract f <?> "interger"
  where
    f (TokInt i) = Just i
    f _ = Nothing

sExpression :: Parser Src.Expr
sExpression = parens expression

expression :: Parser Src.Expr
expression = PC.takeLoc $ choice []
