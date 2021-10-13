module Ulc.WebAssembly.BitDebug
  ( showBits
  , showByte
  , showBytes
  )
  where

import Data.Bits (FiniteBits, finiteBitSize, testBit, shiftR)
import Data.Word (Word8)
import Data.Char (toUpper)
import Data.List (intercalate)
import Numeric (showHex)

showBits :: FiniteBits a => a -> String
showBits bits =
  go (finiteBitSize bits - 1) where
    go shift =
      if shift >= 0
        then
          let bit = if testBit (shiftR bits shift) 0 then '1' else '0'
          in bit : go (pred shift)
        else
          ""

showByte :: Word8 -> String
showByte byte =
  "0x" ++ map toUpper (showHex byte "")

showBytes :: [Word8] -> String
showBytes =
  intercalate " " . map showByte
