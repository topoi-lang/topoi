module Type.Type where

data Type 
  = TFunc {
      arg    :: Type,
      return :: Type
    }
  | TList {
      element :: Type
  }
  | TInt {
      value :: Int
  }