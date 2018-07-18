{-# LANGUAGE MultiWayIf #-}

module HaskellWorks.Data.Json.Internal.Blank2 where

import Data.ByteString
import Data.Char                                   (ord)
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Json.Internal.Mark.Escape
import HaskellWorks.Data.Json.Internal.Mask.Quote
import HaskellWorks.Data.Vector.AsVector64
import Prelude                                     hiding (foldr)

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

wideNot :: [DVS.Vector Word64] -> [DVS.Vector Word64]
wideNot = undefined -- TODO

makeQuoteMaskFromByteString :: [ByteString] -> [DVS.Vector Word64]
makeQuoteMaskFromByteString as = quoteMask
  where quotes      = SIMD.cmpeq8s (fromIntegral (ord '"' )) . asVector64 <$> as
        backslashes = SIMD.cmpeq8s (fromIntegral (ord '\\')) . asVector64 <$> as
        escapees    = markEscapedBits backslashes
        jsonQuotes  = quotes `wideAnd` (wideNot escapees)
        quoteMask   = makeQuoteMask jsonQuotes
