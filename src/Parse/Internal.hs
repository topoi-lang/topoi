{-# LANGUAGE Rank2Types #-}

module Parse.Internal where

-- import qualified Data.ByteString.Internal as B
import Data.Word (Word16) -- UTF-16 ?

type Row = Word16

type Col = Word16

data State
  = State
      { source :: String, -- I should use bytestring
        offset :: Int,
        indent :: Word16,
        row :: Row,
        col :: Col
      }

{-
Manually monoporhism this type
-}
newtype Parser x a
  = Parser
      ( forall b. -- polymorphic function this is
        State ->
        (a -> State -> b) -> -- consumed
        (a -> State -> b) -> -- empty
        (Row -> Col -> (Row -> Col -> x) -> b) -> -- consumed err
        (Row -> Col -> (Row -> Col -> x) -> b) -> -- empty err
        b
      )

{-# INLINE oneOf #-}
oneOf :: (Row -> Col -> x) -> [Parser x a] -> Parser x a
oneOf toError parsers = Parser $ \state consumeOk emptyOk consumeErr emptyErr ->
  oneOfHelp state consumeOk emptyOk consumeErr emptyErr toError parsers

oneOfHelp ::
  State ->
  (a -> State -> b) -> -- consumed
  (a -> State -> b) -> -- empty
  (Row -> Col -> (Row -> Col -> x) -> b) -> -- consumed err
  (Row -> Col -> (Row -> Col -> x) -> b) -> -- empty err
  (Row -> Col -> x) ->
  [Parser x a] ->
  b
oneOfHelp state consumeOk emptyOk consumeErr emptyErr toError parsers =
  case parsers of
    [] ->
      let (State _ _ _ row' col') = state
       in emptyErr row' col' toError
    Parser parser : parsers' ->
      let emptyErr' _ _ _ = oneOfHelp state consumeOk emptyOk consumeErr emptyErr toError parsers'
       in parser state consumeOk emptyOk consumeErr emptyErr'

{-# INLINE oneOfWithFallback #-}
oneOfWithFallback :: [Parser x a] -> a -> Parser x a
oneOfWithFallback parsers fallback =
  Parser $ \state consumeOk emptyOk consumeErr _ ->
    oowfHelp state consumeOk emptyOk consumeErr parsers fallback

{- oowfHelp stands for oneOfWithFallbackHelp -}
oowfHelp ::
  State ->
  (a -> State -> b) ->
  (a -> State -> b) ->
  (Row -> Col -> (Row -> Col -> x) -> b) ->
  [Parser x a] ->
  a ->
  b
oowfHelp state cok eok cerr parsers fallback =
  case parsers of
    [] ->
      eok fallback state
    Parser parser : parsers' ->
      let eerr' _ _ _ = oowfHelp state cok eok cerr parsers' fallback
       in parser state cok eok cerr eerr'

{- In Either monad the Right is ok result -}
-- toOk :: (Row -> Col -> x) -> a -> State -> Either x a
-- toErr

getCol :: Parser x Word16
getCol = Parser $ \state@(State _ _ _ _ col') _ emptyOk _ _ -> emptyOk col' state

-- https://package.elm-lang.org/packages/elm-tools/parser-primitives/latest/ParserPrimitives -

data Position
  = Position
      {-# UNPACK #-} !Word16
      {-# UNPACK #-} !Word16

getPosition :: Parser x Position
getPosition = Parser $ \state@(State _ _ _ row' col') _ emptyOk _ _ -> emptyOk (Position row' col') state
