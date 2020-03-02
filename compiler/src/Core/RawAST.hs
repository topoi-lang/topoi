{-# LANGUAGE OverloadedStrings #-} -- TODO: Should I add the LANGUAGE pragma on cabal files?

module Core.RawAST
    ( Parser
    , SExpr(..)
    ) where

import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String -- TODO: Should I use the String here?

data SExpr
    = Atom String
    | List [SExpr]
    | Number Integer
    | String String
    | Bool Bool
    deriving (Eq, Read, Show)
