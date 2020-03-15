module App.Monad where

import Control.Monad.Reader (ReaderT)

newtype AppT m a
  = AppT
      { unAppT :: ReaderT () m a
      }
  deriving (Functor, Applicative, Monad)
