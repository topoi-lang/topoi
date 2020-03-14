module App.Env where

import Colog (LogAction (..), Message)

-- TODO: this is probably not the best data structure
type OptFlags = HashMap Text Bool

data Env m
  = Env
      { envLogAction :: !(LogAction m Message),
        envOptFlags :: !OptFlags
      }
