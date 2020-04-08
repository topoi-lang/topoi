{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexer where

import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Loc
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Lexer.Applicative hiding (LexicalError)
import qualified Language.Lexer.Applicative as Lexer
import Parse.TokenStream (PrettyToken (..))
import Text.Regex.Applicative

data Tok
  = TokNewline
  | TokWhiteSpace
  | TokEOF
  | TokParenOpen
  | TokParenClose
  | TokAtom Text
  | TokIdent Text
  | TokInt Int
  deriving (Eq, Ord)

instance Show Tok where
  show tok = case tok of
    TokNewline -> "\n"
    TokWhiteSpace -> " "
    TokEOF -> ""
    TokParenOpen -> "("
    TokParenClose -> ")"
    TokAtom s -> Text.unpack s
    TokIdent s -> Text.unpack s
    TokInt i -> show i

text :: Text -> RE Text Text
text rawText = Text.foldr f (pure "") rawText
  where
    f :: Char -> RE Text Text -> RE Text Text
    f c p = Text.cons <$ sym (Text.singleton c) <*> pure c <*> p

-- | Regex rules
tokRE :: RE Text Tok
tokRE =
  TokNewline <$ text "\n"
    <|> TokParenOpen <$ text "("
    <|> TokParenClose <$ text ")"
    <|> TokAtom <$> atomRE
    <|> TokIdent <$> identifierRE
    <|> TokInt <$> intRE

check :: (Char -> Bool) -> Text -> Bool
check f xs
  | Text.null xs = False
  | otherwise = f (Text.head xs)

-- starts with `'` character
atomRE :: RE Text Text
atomRE =
  Text.append <$> psym (check (== '\''))
    <*> (Text.concat <$> many (psym (check (\c -> isAlphaNum c || c == '_' || c == '\''))))

identifierRE :: RE Text Text
identifierRE =
  Text.append <$> psym (check (/= '\''))
    <*> (Text.concat <$> many (psym (check (\c -> isAlphaNum c || c == '_' || c == '\''))))

intRE :: RE Text Int
intRE = read <$> (Text.unpack . Text.concat <$> (some . psym . check $ isDigit))

contra :: RE Text a -> RE Char a
contra = comap Text.singleton

lexer :: Lexer Tok
lexer = token (longest $ contra tokRE)

-- mconcat
-- [ token (longest $ contra tokRE),
-- whitespace (longest $ contra whitespaceButNewlineRE),
-- whitespace (longestShortest (contra commentStartRE) (contra . commentEndRE))
-- ]

-- | Scanning
type LexicalError = Pos

-- A TokenStream is a stream of token to replace list, from regex-applicative package
type TokStream = TokenStream (L Tok)

scan :: FilePath -> Text -> Either LexicalError TokStream
scan filepath = filterError . runLexer lexer filepath . Text.unpack
  where
    filterError :: TokStream -> Either LexicalError TokStream
    filterError TsEof = Right TsEof
    filterError (TsToken l xs) = TsToken l <$> filterError xs
    filterError (TsError (Lexer.LexicalError pos)) = Left pos

instance PrettyToken Tok where
  prettyTokens (x :| []) = fromMaybe ("'" <> show (unLoc x) <> "'") (prettyToken' (unLoc x))
  prettyTokens xs = "\"" <> concatMap (f . unLoc) (NE.toList xs) <> "\""
    where
      f tok = case prettyToken' tok of
        Nothing -> show tok
        Just pretty -> "<" <> pretty <> ">"

{- Have to use Maybe here -}
prettyToken' :: Tok -> Maybe String
prettyToken' tok = case tok of
  TokNewline -> Just "newline"
  TokWhiteSpace -> Just "space"
  TokEOF -> Just "end of file"
  _ -> Nothing
