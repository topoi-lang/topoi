{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Parse.SExpression where

-- import qualified AST.Source as Src
-- import qualified Parse.CharacterCodes as C
-- import Parse.Primitives
-- import qualified Parse.Space as Space
-- import qualified Reporting.Annotation as A
-- import qualified Reporting.Error.Syntax as Err

import qualified AST.Source as AST
import Data.Char
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)

data Token = TokOpen | TokClose | TokSymbol Text | TokInt Int | TokAtom Text

data SExpr
  = SList [SExpr]
  | SSym Text
  | SInt Int
  | SAtom Text
  deriving (Show)

-- NOTE: always use Text package to represent the unicode strings
-- https://stackoverflow.com/questions/47446588/difference-between-data-bytestring-and-data-bytestring-char8

tokenize :: Text -> [Token]
tokenize (T.uncons -> Nothing) = []
tokenize (T.uncons -> Just (x, T.uncons -> Just (y, xs)))
  | x == '\\' && y == 'n' = tokenize xs
tokenize (T.uncons -> Just (x, xs))
  | x == '(' = TokOpen : tokenize xs
  | x == ')' = TokClose : tokenize xs
  | isSpace x = tokenize xs
  | isNumber x = TokInt (fst . fromRight (0, "") . decimal $ consumeAndConcat x isNumber xs) : tokenize (T.dropWhile isNumber xs)
  | isTopoiOperator x =
    if T.head sym == '\'' && T.length sym > 1
      then TokAtom (T.drop 1 sym) : tokenize (T.dropWhile isTopoiOperator xs)
      else TokSymbol sym : tokenize (T.dropWhile isTopoiOperator xs)
  | otherwise = error $ "unexpected character: " ++ [x]
  where
    sym = (consumeAndConcat x isTopoiOperator xs)
    isTopoiOperator :: Char -> Bool
    isTopoiOperator c = isAlphaNum c || elem c ("'=+-*/<>%" :: [Char])
    consumeAndConcat :: Char -> (Char -> Bool) -> Text -> Text
    consumeAndConcat firstChar predicate theRestOfText = T.concat [T.pack [firstChar], T.takeWhile predicate theRestOfText]
tokenize _ = undefined

nestOne :: [Token] -> ([SExpr], [Token])
nestOne [] = ([], [])
nestOne (TokAtom sym : tokens) = ([SAtom sym], tokens)
nestOne (TokSymbol sym : tokens) = ([SSym sym], tokens)
nestOne (TokInt int : tokens) = ([SInt int], tokens)
nestOne (TokOpen : tokens) = let (expressions, tokens') = nestMany [] tokens in ([SList expressions], tokens')
nestOne (TokClose : tokens) = ([], tokens)

nestMany :: [SExpr] -> [Token] -> ([SExpr], [Token])
nestMany prev tokens = case nestOne tokens of
  ([], tokens') -> (prev, tokens')
  (expressions, tokens') -> nestMany (prev ++ expressions) tokens'

nest :: [Token] -> [SExpr]
nest tokens = case nestMany [] tokens of
  (expressions, []) -> expressions
  _ -> error "unregconized expression"

parse :: SExpr -> AST.Expr
parse (SList []) = AST.Unit
parse (SList (fn : xs)) = AST.Function (parseSymbolTakeName fn) (parse <$> xs)
parse (SInt x) = AST.Number x
parse (SAtom x) = AST.Atom x
parse (SSym _) = error "stray symbol in the parsing pass"

parseSymbolTakeName :: SExpr -> Text
parseSymbolTakeName (SSym x) = x
parseSymbolTakeName _ = error "internal error occurred"
