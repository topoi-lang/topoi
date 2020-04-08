module Reporting.Annotation where

import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Loc

data ID = Int

-- | This is to bookkeeping the state of the source location
-- | for error recorvery
type PosLog token = State (LocState token) -- { Identity }

data LocState token
  = LocState
      { currentLoc :: Loc, -- current Loc mark
        lastToken :: Maybe token, -- the last accepted token
        opened :: IntSet, -- waiting to be moved to the "logged" map when the starting position of the next token is determined
        logged :: IntMap Loc, -- waiting to be removed when the ending position is determined
        index :: Int -- for generating fresh ids
      }

runPosLog :: State (LocState token) a -> a
runPosLog f = evalState f (LocState NoLoc Nothing IntSet.empty IntMap.empty 0)

updateLoc :: Loc -> PosLog token ()
updateLoc loc = do
  set <- gets opened
  let addedLoc = IntMap.fromSet (const loc) set
  modify $ \st ->
    st
      { currentLoc = loc,
        opened = IntSet.empty,
        logged = IntMap.union (logged st) addedLoc
      }

updateToken :: token -> PosLog token ()
updateToken tok = modify $ \st -> st {lastToken = Just tok}
