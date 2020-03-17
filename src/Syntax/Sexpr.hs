module Syntax.Sexpr where

-- import Control.Applicative (empty)
import Control.Monad
import Data.Text hiding (empty)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Node
  = Atom Text
  | Word Text
  | Number Int

type Parser = Parsec Void Text

consume :: Parser ()
consume = L.space space1 lineComment empty
  where
    lineComment = L.skipLineComment ";"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme consume

symbol :: Text -> Parser Text
symbol = L.symbol consume

-- TODO:
-- parseSExpr :: Parser Node
-- parseSExpr = try parseInteger <|> parseWord <|> parseAtom

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser Text
identifier = lexeme (T.pack <$> some (alphaNumChar <|> char '-' <|> char '_'))

parseInteger :: Parser Int
parseInteger = lexeme L.decimal

parseWord :: Parser Text
parseWord = identifier <?> "identifier" -- This is for label

parseAtom :: Parser Text
parseAtom = char '\'' *> identifier <?> "atom"
