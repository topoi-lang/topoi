module Parse.Sexpr (Token (..), lexer) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import qualified Data.Text.Lazy as LText
import Data.Text.Lazy (Text)
import Language.Lexer.Applicative (Lexer, longest, token, whitespace)
import Text.Regex.Applicative ((<|>), RE, comap, many, msym, psym, some, sym)

type LText = Text

data Token
  = TokWord LText
  | TokAtom LText
  | TokNumber Int
  | TokParenOpen
  | TokParenClose
  | TokTab
  | TokNewline
  | TokSpace
  deriving (Show, Eq)

text :: LText -> RE LText LText
text raw = LText.foldr f (pure "") raw
  where
    f :: Char -> RE LText LText -> RE LText LText
    f c p = LText.cons <$ sym (LText.singleton c) <*> pure c <*> p

tokRE :: RE LText Token
tokRE =
  TokAtom <$> atomRE
    <|> TokWord <$> wordRE
    <|> TokNumber <$> intRE
    <|> TokParenOpen <$ text "("
    <|> TokParenClose <$ text ")"
    <|> TokTab <$ text "\t"
    <|> TokNewline <$ text "\n"
    <|> TokSpace <$ text " "

adapt :: (Char -> Bool) -> LText -> Bool
adapt f xs
  | LText.null xs = False
  | otherwise = f (LText.head xs)

intRE :: RE LText Int
intRE = read <$> (LText.unpack . LText.concat <$> some (psym (adapt isDigit)))

wordRE :: RE LText LText
wordRE = LText.concat <$> many (psym (adapt (\c -> isAlphaNum c || c == '_' || c == '\'')))

-- atom always starts with a `'`
atomRE :: RE LText LText
atomRE =
  LText.append
    <$> psym (adapt (== '\''))
    <*> (LText.concat <$> many (psym $ adapt (\c -> isAlphaNum c || c == '_' || c == '-')))

whitespaceRE :: RE LText Token
whitespaceRE = matchWhen (adapt isSpace) TokSpace
  where
    matchWhen :: (LText -> Bool) -> Token -> RE LText Token
    matchWhen p symbol = msym (\t -> if p t then Just symbol else Nothing)

lexer :: Lexer Token
lexer =
  mconcat
    [ token (longest . comap LText.singleton $ tokRE),
      whitespace (longest . comap LText.singleton $ whitespaceRE)
    ]
