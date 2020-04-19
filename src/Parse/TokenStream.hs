{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Since we are modifying the megaparsec's stream handling so we write
-- instances here, so is the -no-warn-orphans flag used.

module Parse.TokenStream where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Loc
import Data.Maybe (fromMaybe)
import Data.Proxy
import Language.Lexer.Applicative
import Text.Megaparsec hiding (Pos)

class PrettyToken tok where
  prettyTokens :: NonEmpty (L tok) -> String
  restoreToken :: tok -> String

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
  reachOffset = reachOffset'

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

-- | Given an offset o and initial PosState, adjust the state in such a way
-- that it starts at the offset.reachOffset'
reachOffset' ::
  PrettyToken tok =>
  Int ->
  PosState (TokenStream (L tok)) ->
  (String, PosState (TokenStream (L tok)))
reachOffset' n posState =
  case takeN_' (n - pstateOffset posState) (pstateInput posState) of
    Nothing -> ("<empty line>", posState)
    Just (pre, post) ->
      ( resultLine currentPos (showTokenLines (NE.fromList focusedTokens)),
        newPosState
      )
      where
        filename = sourceName (pstateSourcePos posState)
        currentPos = getCurrentPos filename pre post
        focusedTokens =
          dropWhile (not . (isSameLineAsCurrentPos currentPos)) pre
            ++ fst (takeWhile_' (isSameLineAsCurrentPos currentPos) post)
        newPosState =
          PosState
            { pstateInput = post,
              pstateOffset = max n (pstateOffset posState),
              pstateSourcePos = toSourcePos currentPos,
              pstateTabWidth = pstateTabWidth posState,
              pstateLinePrefix = pstateLinePrefix posState
            }

getCurrentPos :: FilePath -> [L tok] -> TokenStream (L tok) -> Pos
getCurrentPos filename pre post = case post of
  -- starting position of the first token of `post`
  TsToken (L (Loc start _) _) _ -> start
  -- end of stream, use the position of the last token from `pre` instead
  _ -> case (length pre, last pre) of
    (0, _) -> Pos filename 1 1 0
    (i, L NoLoc _) -> Pos filename 1 1 i
    (_, L (Loc _ end) _) -> end

getLineSpan :: L tok -> Maybe (Int, Int)
getLineSpan (L NoLoc _) = Nothing
getLineSpan (L (Loc start end) _) = Just (posLine start, posLine end)

isSameLineAsCurrentPos :: Pos -> L tok -> Bool
isSameLineAsCurrentPos currentPos tok = case getLineSpan tok of
  Nothing -> False
  Just (x, y) -> x <= posLine currentPos && y >= posLine currentPos

resultLine :: Pos -> [(Int, String)] -> String
resultLine currentPos focusedLines =
  fromMaybe "<line not found>" $ lookup (posLine currentPos) focusedLines

toSourcePos :: Pos -> SourcePos
toSourcePos (Pos filename line column _) =
  SourcePos filename (mkPos line) (mkPos column)

data Chunk
  = Chunk
      (Int, Int) -- start
      (NonEmpty String) -- payload
      (Int, Int) -- end
  deriving (Show)

-- returns lines + line numbers of the string representation of tokens
showTokenLines :: PrettyToken tok => NonEmpty (L tok) -> [(Int, String)]
showTokenLines (x :| xs) = zip lineNumbers (NE.toList body)
  where
    glued :: Chunk
    glued = foldl glue (toChunk x) (fmap toChunk xs)
    Chunk start body _ = glued
    lineNumbers :: [Int]
    lineNumbers = [(fst start) + 1 ..]

toChunk :: PrettyToken tok => L tok -> Chunk
toChunk (L NoLoc tok) = Chunk start strings end
  where
    strings = case nonEmpty (lines (restoreToken tok)) of
      Nothing -> "" :| []
      Just xs -> xs
    start = (0, 0)
    end = (NE.length strings - 1, length (NE.last strings))
toChunk (L (Loc from to) tok) = Chunk start strings end
  where
    strings = case nonEmpty (lines (restoreToken tok)) of
      Nothing -> "" :| []
      Just xs -> xs
    start = (posLine from - 1, posCol from - 1)
    end = (posLine to - 1, posCol to)

glue :: Chunk -> Chunk -> Chunk
glue (Chunk start1 body1 end1) (Chunk start2 body2 end2) =
  Chunk start1 body end2
  where
    lineGap :: Int
    lineGap = fst start2 - fst end1
    body :: NonEmpty String
    body = case lineGap of
      -- two chunk meet at the same line
      0 ->
        let (line :| body2') = body2
            colGap = snd start2 - snd end1
            line' = NE.last body1 ++ replicate colGap ' ' ++ line
         in case nonEmpty (NE.init body1) of
              Nothing -> line' :| body2'
              Just body1' -> body1' <> (line' :| body2')
      -- two chunk differs by one line
      1 ->
        let (line :| body2') = body2
            colGap = snd start2
            line' = replicate colGap ' ' ++ line
         in body1 <> (line' :| body2')
      -- two chunk differs by more than one line
      n ->
        let (line :| body2') = body2
            colGap = snd start2
            line' = replicate colGap ' ' ++ line
            emptyLines = NE.fromList (replicate (n - 1) "")
         in body1 <> emptyLines <> (line' :| body2')
