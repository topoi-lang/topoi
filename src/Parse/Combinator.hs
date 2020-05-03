module Parse.Combinator where

import Control.Monad.State
import Data.Loc
import qualified Data.Set as Set
import Data.Void
import Language.Lexer.Applicative (TokenStream)
import Parse.TokenStream (PrettyToken)
import Reporting.Annotation
import Text.Megaparsec

type P token = ParsecT Void (TokenStream (L token)) (PosLog token)

getLoc :: (Ord tok, PrettyToken tok) => P tok a -> P tok (a, Loc)
getLoc parser = do
  posID <- lift markStart
  result <- parser
  loc <- lift (markEnd posID)
  pure (result, loc)

withLoc :: (Ord tok, PrettyToken tok) => P tok (Loc -> a) -> P tok a
withLoc parser = do
  (result, loc) <- getLoc parser
  pure (result loc)

symbol :: (Ord tok, PrettyToken tok) => tok -> P tok ()
symbol tok = do
  L loc token' <- satisfy (\(L _ t) -> tok == t)
  lift $ updateLoc loc
  lift $ updateToken token'
  pure ()

-- parse but didn't update the source location (excluding it from source
-- location tracking)
ignore :: (Ord tok, PrettyToken tok) => tok -> P tok ()
ignore tok = do
  L _ token' <- satisfy (\(L _ t) -> tok == t)
  lift $ updateToken token'
  pure ()

extract :: (Ord tok, PrettyToken tok) => (tok -> Maybe a) -> P tok a
extract f = do
  (result, token', loc) <- token p Set.empty
  lift $ updateLoc loc
  lift $ updateToken token'
  pure result
  where
    p (L loc tok) = case f tok of
      Just result -> Just (result, tok, loc)
      Nothing -> Nothing

between ::
  (Ord tok, PrettyToken tok, Relocatable c) =>
  P tok a ->
  P tok b ->
  P tok c ->
  P tok c
between left right parser = do
  (_, start) <- getLoc left
  result <- parser
  (_, end) <- getLoc right
  pure $ reloc (start <--> end) result -- combine the start and end into span
