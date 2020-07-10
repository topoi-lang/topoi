{-# LANGUAGE TemplateHaskell #-}

module Topoi.Compiler where

import Control.Monad.IO.Class
import Data.GADT.Compare.TH (deriveGEq)
import Data.IORef
import Data.Some
import qualified Rock

data Query a where
    Greet :: Query String

deriving instance Show (Query a)
deriveGEq ''Query

rules :: Rock.Rules Query
rules key = do
    liftIO . putStrLn $ "Fetching " <> show key
    case key of
        Greet -> pure "10"

print'' :: IO ()
print'' = putStrLn "hello"