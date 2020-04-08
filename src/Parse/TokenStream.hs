{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Since we are modifying the megaparsec's stream handling so we write
-- instances here, so is the -no-warn-orphans flag used.

module Parse.TokenStream where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Loc
import Data.Proxy
import Language.Lexer.Applicative
import Text.Megaparsec hiding (Pos)

class PrettyToken tok where
  prettyTokens :: NonEmpty (L tok) -> String

instance Ord tok => Ord (TokenStream (L tok)) where
  compare _ _ = EQ -- always EQ ?

instance (Ord tok, PrettyToken tok) => Stream (TokenStream (L tok)) where
  -- type of token in the stream, defined in the Text.Megaparsec.Stream
  type Token (TokenStream (L tok)) = L tok

  -- type of "chunks" of the stream
  type Tokens (TokenStream (L tok)) = [L tok]

  tokenToChunk Proxy tok = [tok]
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = chunkLength'
  chunkEmpty Proxy = chunkEmpty'
  take1_ = take1_'
  takeN_ = takeN_'
  takeWhile_ = takeWhile_'
  showTokens Proxy = prettyTokens
  reachOffset = undefined

-- | Return length of a chunk of the stream
chunkLength' :: [L tok] -> Int
chunkLength' = length

-- | Check if a chunk of the stream is empty
chunkEmpty' :: [L tok] -> Bool
chunkEmpty' toks = (chunkLength' toks) == 0

-- | Extract a single token form the stream
take1_' :: TokenStream (L tok) -> Maybe (L tok, (TokenStream (L tok)))
take1_' (TsToken tok restOfTheToken) = Just (tok, restOfTheToken)
take1_' _ = Nothing

streamEmpty :: TokenStream (L tok) -> Bool
streamEmpty (TsToken _ _) = False
streamEmpty TsEof = True
streamEmpty (TsError _) = True

takeN_' :: Int -> TokenStream (L tok) -> Maybe ([L tok], (TokenStream (L tok)))
takeN_' n s
  | n <= 0 = Just ([], s)
  | streamEmpty s = Just ([], s)
  | otherwise = Just (go n s)
  where
    go :: Int -> TokenStream (L tok) -> ([L tok], TokenStream (L tok))
    go _ TsEof = ([], TsEof)
    go _ (TsError _) = ([], TsEof)
    go 0 (TsToken x xs) = ([], TsToken x xs)
    go i (TsToken x xs) = let (ys, zs) = go (i - 1) xs in (x : ys, zs)

-- | Extra chunk of the stream taking tokens while the supplied predicate
-- returns 'True'. Return the chunk and the rest of the stream.
takeWhile_' :: (L tok -> Bool) -> TokenStream (L tok) -> ([L tok], TokenStream (L tok))
takeWhile_' predicate stream = case take1_' stream of
  Nothing -> ([], stream)
  Just (x, rest) ->
    if predicate x
      then let (xs, rest') = takeWhile_' predicate rest in (x : xs, rest')
      else ([], stream)
