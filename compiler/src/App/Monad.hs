module App.Monad where

newtype AppT m a
  = AppT
      { unAppT :: ReaderT () m a
      }
  deriving (Functor, Applicative, Monad)
