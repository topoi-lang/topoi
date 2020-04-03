{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Parse.SExpression where

-- import qualified AST.Source as Src
-- import qualified Parse.CharacterCodes as C
-- import Parse.Primitives
-- import qualified Parse.Space as Space
-- import qualified Reporting.Annotation as A
-- import qualified Reporting.Error.Syntax as Err

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Either
import Data.Text.Read (decimal)
import Data.Char

data Token = TokOpen | TokClose | TokSymbol Text | TokInt Int

data Expression
  = ExpList [Expression]
  | ExpSymbol Text
  | ExpInt Int
  -- | ExpAtom Text  -- shalt we ?
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
  | isTopoiOperator x = TokSymbol (consumeAndConcat x isTopoiOperator xs) : tokenize (T.dropWhile isTopoiOperator xs)
  | isNumber x = TokInt (fst . fromRight (0, "") . decimal $ consumeAndConcat x isNumber xs) : tokenize (T.dropWhile isNumber xs)
  | otherwise = error $ "unexpected character: " ++ [x]
  where
    isTopoiOperator :: Char -> Bool
    isTopoiOperator c = isAlphaNum c || elem c ("'=+-*/<>%" :: [Char])

    consumeAndConcat :: Char -> (Char -> Bool) -> Text -> Text
    consumeAndConcat firstChar predicate theRestOfText = T.concat [T.pack [firstChar], T.takeWhile predicate theRestOfText]
tokenize _ = undefined

nestOne :: [Token] -> ([Expression], [Token])
nestOne [] = ([], [])
nestOne (TokSymbol sym : tokens) = ([ExpSymbol sym], tokens)
nestOne (TokInt int : tokens) = ([ExpInt int], tokens)
nestOne (TokOpen : tokens) = let (expressions, tokens') = nestMany [] tokens in ([ExpList expressions], tokens')
nestOne (TokClose : tokens) = ([], tokens)

nestMany :: [Expression] -> [Token] -> ([Expression], [Token])
nestMany prev tokens = case nestOne tokens of
  ([], tokens') -> (prev, tokens')
  (expressions, tokens') -> nestMany (prev ++ expressions) tokens'

nest :: [Token] -> Expression
nest tokens = case nestMany [] tokens of
  (expressions, []) -> ExpList $ ExpSymbol "Seq" : expressions
  _ -> error "unregconized expression"
