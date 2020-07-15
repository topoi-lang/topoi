{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Topoi.Compiler.Parser where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import GHC.Exts
import System.IO.Unsafe
import GHC.Word
import Data.Word8
import Data.Char (ord)
import Data.Bits hiding (shift)
import Data.Foldable
import Language.Haskell.TH
import System.IO.Unsafe

data Error e = Default | Custom e
    deriving (Show)

type Result# e a = (# (# a, Addr# #) | (# Error e, Addr# #) #)

pattern OK# :: a -> Addr# -> Result# e a
pattern OK# a addr = (# (# a, addr #) | #)

pattern Err# :: Error e -> Addr# -> Result# e a
pattern Err# err addr = (# | (# err, addr #) #)

-- | Get byte length of code point from the first byte
getUTF8Len# :: Word# -> Int#
getUTF8Len# w = word2Int# (clz8# (not# w))
{-# INLINE getUTF8Len# #-}

newtype Parser e a = Parser { runParser# :: Addr# -> Addr# -> Result# e a }

instance Functor (Parser e) where
    fmap f (Parser g) = Parser \r s -> case g r s of
        OK# a s  -> let !b = f a in OK# b s
        Err# e s -> Err# e s
    {-# inline fmap #-}
    
    -- | returns the value from the previous input
    (<$) a' (Parser g) = Parser \r s -> case g r s of
        OK# a s -> OK# a' s
        Err# e s -> Err# e s
    {-# INLINE (<$) #-}

instance Applicative (Parser e) where
    pure a = Parser \r s -> OK# a s
    {-# INLINE pure #-}

    Parser ff <*> Parser fa = Parser \r s -> case ff r s of
        Err# e s -> Err# e s
        OK# f s -> case fa r s of
            Err# e s -> Err# e s
            OK# a s -> OK# (f a) s
    {-# INLINE (<*>) #-}

    Parser fa <* Parser fb = Parser \r s -> case fa r s of
        Err# e s -> Err# e s
        OK# a s -> case fb r s of
            Err# e s -> Err# e s
            OK# b s -> OK# a s
    {-# inline (<*) #-}

    Parser fa *> Parser fb = Parser \r s -> case fa r s of
        Err# e s -> Err# e s
        OK# a s  -> fb r s
    {-# inline (*>) #-}

instance Monad (Parser e) where
    return = pure
    {-# INLINE return #-}

    Parser fa >>= f = Parser \r s -> case fa r s of
        Err# e s -> Err# e s
        OK# a s -> runParser# (f a) r s
    {-# INLINE (>>=) #-}

    Parser fa >> Parser fb = Parser \r s -> case fa r s of
        Err# e s -> Err# e s
        OK# a s -> fb r s
    {-# INLINE (>>) #-}

err :: e -> Parser e a
err e = Parser \r s -> Err# (Custom e) s
{-# INLINE err #-}

empty :: Parser e a
empty = Parser \r s -> Err# Default s
{-# INLINE empty #-}

eof :: Parser e ()
eof = Parser \eob s -> case eqAddr# eob s of
  1# -> OK# () s
  _  -> Err# Default s
{-# inline eof #-}

-- | The second parser is tried if the first one fails, regardless of how much
-- input the first parser consumed
infixr 6 <!>
(<!>) :: Parser e a -> Parser e a -> Parser e a
(<!>) (Parser f) (Parser g) = Parser \r s -> case f r s of
    Err# e _ -> g r s
    x -> x
{-# INLINE (<!>) #-}

-- | The second parser is tried if the first one consumed empty, or reached EOF
infixr 6 <|>
(<|>) :: Parser e a -> Parser e a -> Parser e a
(<|>) (Parser f) (Parser g) = Parser \r s -> case f r s of
    Err# e s' -> case eqAddr# s s' of
        1# -> g r s
        _ -> Err# e s'
    x -> x
{-# INLINE (<|>) #-}

isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
{-# INLINE isLatinLetter #-}

derefChar8# :: Addr# -> Char#
derefChar8# addr = indexCharOffAddr# addr 0#
{-# INLINE derefChar8# #-}

someTok_ :: Parser e a -> Parser e ()
someTok_ pa = pa >> manyTok_ pa
{-# INLINE someTok_ #-}

manyTok_ :: Parser e a -> Parser e ()
manyTok_ (Parser f) = go where
  go = Parser \r s -> case f r s of
    Err# _ _ -> OK# () s
    OK# a s  -> runParser# go r s
{-# INLINE manyTok_ #-}

-- | Skip a parser one or more times. This fails if the given parser fails with
--   having consumed input.
some_ :: Parser e a -> Parser e ()
some_ pa = pa >> many_ pa
{-# INLINE some_ #-}

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyA :: (Char -> Bool) -> Parser e Char
satisfyA f = Parser \r s -> case runParser# anyCharA r s of
  OK# c s | f c -> OK# c s
  OK# c _       -> Err# Default s
  Err# e s      -> Err# e s
{-# INLINE satisfyA #-}

-- | Skip a parser zero or more times. This fails if the given parser fails with
--   having consumed input.
many_ :: Parser e a -> Parser e ()
many_ (Parser f) = go where
  go = Parser \r s -> case f r s of
    Err# e s' -> case eqAddr# s s' of
                   1# -> OK# () s
                   _  -> Err# e s'
    OK# a s   -> runParser# go r s
{-# INLINE many_ #-}

-- | Parse any `Char` in the ASCII range.
anyCharA :: Parser e Char
anyCharA = Parser \eob buf -> case eqAddr# eob buf of
  1# -> Err# Default buf
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# (C# c1) (plusAddr# buf 1#)
      _  -> Err# Default buf
{-# INLINE anyCharA #-}

br :: Parser e a -> Parser e b -> Parser e b -> Parser e b
br pa pt pf = Parser \r s -> case runParser# pa r s of
    OK# _ s  -> runParser# pt r s
    Err# _ _ -> runParser# pf r s

charToBytes :: Char -> [Word8]
charToBytes c'
    | c <= 0x7f     = [fromIntegral c]
    | c <= 0x7ff    = [0xc0 .|. y, 0x80 .|. z]
    | c <= 0xffff   = [0xe0 .|. x, 0x80 .|. y, 0x80 .|. z]
    | c <= 0x10ffff = [0xf0 .|. w, 0x80 .|. x, 0x80 .|. y, 0x80 .|. z]
    | otherwise = error "Not a valid Unicode code point"
  where
    c = ord c'
    z = fromIntegral (c                 .&. 0x3f)
    y = fromIntegral (unsafeShiftR c 6  .&. 0x3f)
    x = fromIntegral (unsafeShiftR c 12 .&. 0x3f)
    w = fromIntegral (unsafeShiftR c 18 .&. 0x7)

stringToBytes :: [Char] -> [Word8]
stringToBytes = concatMap charToBytes
{-# inline stringToBytes #-}

packBytes :: [Word8] -> Word
packBytes = fst . foldl' go (0, 0) where
    go (_acc, shift) _w | shift == 64 = error "packWords: too many bytes"
    go (acc, shift) w = (unsafeShiftL (fromIntegral w) shift .|. acc, shift+8)

splitBytes :: [Word8] -> ([Word8], [Word])
splitBytes ws = case quotRem (length ws) 8 of
    (0, _) -> (ws, [])
    (_, r) -> (as, chunk8s bs) where
        (as, bs) = splitAt r ws
        
        chunk8s [] = []
        chunk8s ws = do
            let (main, remaining) = splitAt 8 ws
            packBytes main : chunk8s remaining

scanAny8# :: Parser e Word8
scanAny8# = Parser \r s -> OK# (W8# (indexWord8OffAddr# s 0#)) (plusAddr# s 1#)
{-# inline scanAny8# #-}

scan8# :: Word -> Parser e ()
scan8# (W# c) = Parser \r s ->
  case indexWord8OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 1#)
      _  -> Err# Default s
{-# inline scan8# #-}

scan16# :: Word -> Parser e ()
scan16# (W# c) = Parser \r s ->
  case indexWord16OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 2#)
      _  -> Err# Default s
{-# inline scan16# #-}

scan32# :: Word -> Parser e ()
scan32# (W# c) = Parser \r s ->
  case indexWord32OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 4#)
      _  -> Err# Default s
{-# inline scan32# #-}

scan64# :: Word -> Parser e ()
scan64# (W# c) = Parser \r s ->
  case indexWord64OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 8#)
      _  -> Err# Default s
{-# inline scan64# #-}

scanPartial64# :: Int -> Word -> Parser e ()
scanPartial64# (I# len) (W# w) = Parser \r s ->
  case indexWordOffAddr# s 0# of
    w' -> case uncheckedIShiftL# (8# -# len) 3# of
      sh -> case uncheckedShiftL# w' sh of
        w' -> case uncheckedShiftRL# w' sh of
          w' -> case eqWord# w w' of
            1# -> OK# () (plusAddr# s len)
            _  -> Err# Default s
{-# inline scanPartial64# #-}

ensureBytes# :: Int -> Parser e ()
ensureBytes# (I# len) = Parser \eob s ->
  case len  <=# minusAddr# eob s of
    1# -> OK# () s
    _  -> Err# Default s
{-# inline ensureBytes# #-}

scanBytes# :: Bool -> [Word8] -> Q Exp
scanBytes# isToken bytes = do
  let !(leading, w8s) = splitBytes bytes
      !leadingLen     = length leading
      !w8sLen         = length w8s
      !scanw8s        = go w8s where
                         go (w8:[] ) = [| scan64# w8 |]
                         go (w8:w8s) = [| scan64# w8 >> $(go w8s) |]
                         go []       = [| pure () |]
  case w8s of
    [] -> if elem leadingLen [3, 5, 6, 7] && isToken
             then [| try $(go leading) |]
             else [| $(go leading) |]
          where
            go (a:b:c:d:[]) = let !w = packBytes [a, b, c, d] in [| scan32# w |]
            go (a:b:c:d:ws) = let !w = packBytes [a, b, c, d] in [| scan32# w >> $(go ws) |]
            go (a:b:[])     = let !w = packBytes [a, b]       in [| scan16# w |]
            go (a:b:ws)     = let !w = packBytes [a, b]       in [| scan16# w >> $(go ws) |]
            go (a:[])       = [| scan8# a |]
            go []           = [| pure () |]
    _  -> case leading of

      [] | w8sLen /= 1 && isToken -> [| try $scanw8s |]
         | otherwise              -> [| $scanw8s     |]

      [a] | isToken   -> [| try (scan8# a >> $scanw8s) |]
          | otherwise -> [| scan8# a >> $scanw8s |]

      ws  -> let !w = packBytes ws
                 !l = length ws
             in if isToken then [| try (scanPartial64# l w >> $scanw8s) |]
                           else [| scanPartial64# l w >> $scanw8s |]

string :: String -> Q Exp
string str = do
  let !bytes = stringToBytes str
      !len   = length bytes
  [| ensureBytes# len >> $(scanBytes# True bytes) |]

char :: Char -> Q Exp
char c = string [c]
