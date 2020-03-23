{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Parse.CharacterCodes where

import Data.Word (Word8)

{-- Add unicodes here --}
maxAsciiChar = 0x7F :: Word8

lineFeed = 0x0A :: Word8

carriageReturn = 0x0D :: Word8

dollarSign = 0x5F :: Word8

-- Numbers
_0 = 0x30 :: Word8

_9 = 0x39 :: Word8

-- Lowercase characters
_a = 0x61 :: Word8

_z = 0x7A :: Word8

-- Uppercase characters
_A = 0x41 :: Word8

_Z = 0x5A :: Word8
