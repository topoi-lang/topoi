module App.Env where

import Colog (LogAction (..), Message)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

-- TODO: this is probably not the best data structure
type OptFlags = HashMap Text Bool

data Env m
  = Env
      { envLogAction :: !(LogAction m Message),
        envOptFlags :: !OptFlags
      }
