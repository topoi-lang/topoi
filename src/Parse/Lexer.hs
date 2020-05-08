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
  | TokUpperIdent Text
  | TokLowerIdent Text
  | TokInt Int
  | TokAssign
  | TokSemicolon
  | TokLeftArrow
  | TokDocLineComment Text
  deriving (Eq, Ord)

instance Show Tok where
  show tok = case tok of
    TokNewline -> "\n"
    TokWhiteSpace -> " "
    TokEOF -> ""
    TokParenOpen -> "("
    TokParenClose -> ")"
    TokUpperIdent s -> show s
    TokLowerIdent s -> show s
    TokInt i -> show i
    TokAssign -> "define"
    TokSemicolon -> ":"
    TokLeftArrow -> "->"
    TokDocLineComment s -> "--|" <> show s

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
    <|> TokAssign <$ text "define"
    <|> TokLowerIdent <$> lowercaseIdentifierRE
    <|> TokUpperIdent <$> uppercaseIdentifierRE
    <|> TokInt <$> intRE
    <|> TokSemicolon <$ text ":"
    <|> TokLeftArrow <$ text "->"

check :: (Char -> Bool) -> Text -> Bool
check f xs
  | Text.null xs = False
  | otherwise = f (Text.head xs)

-- starts with `'` character
atomRE :: RE Text Text
atomRE =
  Text.append <$> psym (check (== '\''))
    <*> (Text.concat <$> many (psym (check (\c -> isAlphaNum c || c == '_'))))

identifierRE :: RE Text Text
identifierRE =
  Text.append <$> psym (check isAlpha)
    <*> (Text.concat <$> many (psym (check (\c -> isAlphaNum c || c == '_'))))

uppercaseIdentifierRE :: RE Text Text
uppercaseIdentifierRE =
  Text.append <$> psym (check isUpper) -- it actually check the Unicode!
    <*> (Text.concat <$> many (psym (check (\c -> isAlphaNum c || c == '_'))))

lowercaseIdentifierRE :: RE Text Text
lowercaseIdentifierRE =
  Text.append <$> psym (check isLower) -- it actually check the Unicode!
    <*> (Text.concat <$> many (psym (check (\c -> isAlphaNum c || c == '_'))))

intRE :: RE Text Int
intRE =
  read
    <$> (Text.unpack . Text.concat <$> (some . psym . check $ isDigit))

contra :: RE Text a -> RE Char a
contra = comap Text.singleton

whitespaceButNewlineRE :: RE Text Tok
whitespaceButNewlineRE =
  matchWhen
    (check (\c -> isSpace c && c /= '\n' && c /= '\r'))
    TokWhiteSpace
  where
    matchWhen :: (Text -> Bool) -> Tok -> RE Text Tok
    matchWhen p symbol = msym (\t -> if p t then Just symbol else Nothing)

docLineCommentRE :: Text -> RE Text Tok
docLineCommentRE _prefix =
  TokDocLineComment <$> (Text.concat <$> many anySym) +++ (text "\n")
  where
    (+++) = liftA2 (<>)

lexer :: Lexer Tok
lexer =
  mconcat
    [ token (longest $ contra tokRE),
      whitespace (longest $ contra whitespaceButNewlineRE),
      whitespace (longestShortest (contra $ text "--") (contra . (\_ -> many anySym *> text "\n"))),
      whitespace (longestShortest (contra $ text "--[") (contra . (\_ -> many anySym *> text "]--"))),
      token (longestShortest (contra $ text "--|") (contra . docLineCommentRE))
    ]

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
  restoreToken = show

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
