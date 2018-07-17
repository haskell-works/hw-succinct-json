{-# LANGUAGE MultiWayIf #-}

module HaskellWorks.Data.Json.Internal.Blank2 where

import Data.ByteString
import Data.Char                           (ord)
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Vector.AsVector64

import qualified Data.Vector.Storable              as DVS
import qualified HaskellWorks.Data.Simd.Comparison as SIMD

makeQuoteMaskChunk :: ByteString -> DVS.Vector Word64
makeQuoteMaskChunk = SIMD.cmpeq8s (fromIntegral (ord '"')) . asVector64

shiftOneBitL' :: Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
shiftOneBitL' wLast v = DVS.constructN (fromIntegral vLen) go
  where vLen = end v
        go u = if i < vLen - 1
          then (vj .>. 1) .|. (vk     .<. 63)
          else (vj .>. 1) .|. (wLast  .<. 63)
          where i  = end u
                vj = v !!! i
                vk = v !!! (i + 1)

shiftOneBitL :: [DVS.Vector Word64] -> [DVS.Vector Word64]
shiftOneBitL (a:b:bs) = if
  | DVS.null a -> shiftOneBitL (b:bs)
  | DVS.null b -> shiftOneBitL (a:bs)
  | otherwise  -> shiftOneBitL' ((b !!! 0) .&. 1) a:shiftOneBitL (b:bs)
shiftOneBitL [b] = [shiftOneBitL' 0 b]
shiftOneBitL [] = []

shiftOneBitR :: [DVS.Vector Word64] -> [DVS.Vector Word64]
shiftOneBitR = undefined -- TODO

wideAnd :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
wideAnd = undefined -- TODO

wideOr :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
wideOr = undefined -- TODO

everyOddOne :: [DVS.Vector Word64] -> [DVS.Vector Word64]
everyOddOne = undefined -- TODO

makeQuoteMask :: [ByteString] -> [DVS.Vector Word64]
makeQuoteMask as = [] -- TODO
  where quotes          = SIMD.cmpeq8s (fromIntegral (ord '"')) . asVector64 <$> as
        backslashO      = SIMD.cmpeq8s (fromIntegral (ord '\\')) . asVector64 <$> as
        backslashL      = shiftOneBitL backslashO
        backslashR      = shiftOneBitR backslashO
        backslashOnL    = backslashO `wideAnd` backslashL
        backslashOnR    = backslashO `wideAnd` backslashL
        backslashChains = backslashOnL `wideOr` backslashOnR
        escapers        = everyOddOne backslashChains

escapedBitsInWord :: Bool -> Word8 -> (Word8, Bool)
escapedBitsInWord c w = go 0 0 c
  where go :: Count -> Word8 -> Bool -> (Word8, Bool)
        go n acc c = if n < 8
          then if c
            then go (n + 1) ((1 .<. n) .|. acc) False
            else go (n + 1) acc                 (((1 .<. n) .&. w) /= 0)
          else (acc, c)

escapedBitsMap0 :: DVS.Vector Word8
escapedBitsMap0 = DVS.constructN 256 go
  where go :: DVS.Vector Word8 -> Word8
        go u = fst (escapedBitsInWord False (fromIntegral (DVS.length u)))
{-# NOINLINE escapedBitsMap0 #-}

escapedBitsMap1 :: DVS.Vector Word8
escapedBitsMap1 = DVS.constructN 256 go
  where go :: DVS.Vector Word8 -> Word8
        go u = fst (escapedBitsInWord True (fromIntegral (DVS.length u)))
{-# NOINLINE escapedBitsMap1 #-}

escapedBits' :: DVS.Vector Word64 -> (DVS.Vector Word64, DVS.Vector Word64)
escapedBits' v = undefined

-- escapedBits :: [DVS.Vector Word64] -> [DVS.Vector Word64]
-- escapedBits v =
--     DVS.unsafeCast v
