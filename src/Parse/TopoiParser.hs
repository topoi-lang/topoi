{-# LANGUAGE OverloadedStrings #-}

module Parse.TopoiParser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Loc
import Data.Text (Text)
import Data.Void
import qualified Parse.Combinator as P
import Parse.Lexer
import Reporting.Annotation (PosLog)
import qualified Reporting.Annotation as A
import Syntax.Concrete
import qualified Text.Megaparsec as MP
import Text.Megaparsec hiding (ParseError, Pos, State, parse)

{- Note: Topoi Parser
~~~~~~~~~~~~~~~~~~~~~~

This will parse the source unit and output the structured data defined in
`src/Syntax/Concrete.hs`

If you want to modify the comment parser, please check `src/Parse/Lexer.hs`

-}

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
  P.symbol t
  void $ many (P.ignore TokNewline)

parens :: Parser a -> Parser a
parens =
  between
    (symbol TokParenOpen <?> "left parenthesis")
    (symbol TokParenClose <?> "right parenthesis")

-- | This means uppercase identifier name
data Upper = Upper Text Loc deriving (Show)

-- | This means lowercase identifier name
data Lower = Lower Text Loc deriving (Show)

lowercaseIdentifierName :: Parser Text
lowercaseIdentifierName = P.extract f
  where
    f (TokLowerIdent s) = Just s
    f _ = Nothing

uppercaseIdentifierName :: Parser Text
uppercaseIdentifierName = P.extract f
  where
    f (TokUpperIdent s) = Just s
    f _ = Nothing
-- typeSignature :: Parser Statement
-- typeSignature = P.withLoc $ do
--   name <- P.withLoc $ Lower <$> lowercaseIdentifierName
--   symbol TokSemicolon <?> ":"
--   TypeDecl name <$> typeParse

-- typeParse :: Parser Type
-- typeParse = makeExprParser term table <?> "type"
--   where
--     table :: [[Operator Parser Type]]
--     table = [[InfixR functionTypeParse]]
--     term = parens typeParse <|> baseTypeParse

-- functionTypeParse :: Parser (Type -> Type -> Type)
-- functionTypeParse = do
--   symbol TokLeftArrow <?> "->"
--   return (\x y -> TFunc x y (x <--> y))

-- baseTypeParse :: Parser Type
-- baseTypeParse = P.withLoc (TBase <$> P.extract isBaseType) <?> "base type"
--   where
--     isBaseType (TokUpperIdent "Int") = Just TInt
--     isBaseType (TokUpperIdent "Bool") = Just TBool
--     isBaseType (TokUpperIdent "Char") = Just TChar
--     isBaseType _ = Nothing
