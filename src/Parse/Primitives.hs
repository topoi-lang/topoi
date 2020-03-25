{-# LANGUAGE Rank2Types #-}

module Parse.Primitives where

-- import qualified Data.ByteString.Internal as B

import qualified Data.ByteString.Internal as B
import Data.Word (Word16, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import qualified Reporting.Annotation as A

type Row = Word16

type Col = Word16

data State
  = State
      { sSrc :: B.ByteString, -- I should use bytestring
        sPos :: !Word8,
        sEnd :: !Word8,
        sIndent :: !Word16,
        sRow :: !Row,
        sCol :: !Col
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

instance Functor (Parser x) where
  fmap f (Parser parser) = Parser $ \state cok eok cerr eerr ->
    let cok' a s = cok (f a) s
        eok' a s = eok (f a) s
     in parser state cok' eok' cerr eerr

{-# INLINE oneOf #-}
oneOf :: (Row -> Col -> x) -> [Parser x a] -> Parser x a
oneOf toError parsers = Parser $ \state cok eok cerr eerr ->
  oneOfHelp state cok eok cerr eerr toError parsers

oneOfHelp ::
  State ->
  (a -> State -> b) ->
  (a -> State -> b) ->
  (Row -> Col -> (Row -> Col -> x) -> b) ->
  (Row -> Col -> (Row -> Col -> x) -> b) ->
  (Row -> Col -> x) ->
  [Parser x a] ->
  b
oneOfHelp state cok eok cerr eerr toError parsers =
  case parsers of
    Parser parser : parsers' ->
      let eerr' _ _ _ =
            oneOfHelp state cok eok cerr eerr toError parsers'
       in parser state cok eok cerr eerr'
    [] ->
      let (State _ _ _ _ row col) = state
       in eerr row col toError

{-# INLINE oneOfWithFallback #-}
oneOfWithFallback :: [Parser x a] -> a -> Parser x a
oneOfWithFallback parsers fallback = Parser $ \state cok eok cerr _ ->
  oowfHelp state cok eok cerr parsers fallback

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
      let eerr' _ _ _ =
            oowfHelp state cok eok cerr parsers' fallback
       in parser state cok eok cerr eerr'

getPosition :: Parser x A.Position
getPosition = Parser $ \state@(State _ _ _ _ row col) _ eok _ _ ->
  eok (A.Position row col) state

addEnd :: A.Position -> a -> Parser x (A.Located a)
addEnd start value = Parser $ \state@(State _ _ _ _ row col) _ eok _ _ ->
  eok (A.at start (A.Position row col) value) state

unsafeIndex :: Ptr Word8 -> Word8
unsafeIndex ptr = B.accursedUnutterablePerformIO (peek ptr)
