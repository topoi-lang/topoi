-- {-# LANGUAGE OverloadedStrings #-}

module Parse.TokenStream where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Loc

class PrettyToken tok where
  prettyTokens :: NonEmpty (L tok) -> String
