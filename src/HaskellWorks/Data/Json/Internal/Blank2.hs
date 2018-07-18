{-# LANGUAGE MultiWayIf #-}

module HaskellWorks.Data.Json.Internal.Blank2 where

import Control.Monad
import Control.Monad.ST                    (ST)
import Data.ByteString
import Data.Char                           (ord)
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Vector.AsVector64
import Prelude                             hiding (foldr)

import qualified Data.Vector                       as DV
import qualified Data.Vector.Storable              as DVS
import qualified Data.Vector.Storable.Mutable      as DVSM
import qualified HaskellWorks.Data.Simd.Comparison as SIMD
import qualified Prelude                           as P
import qualified System.IO                         as IO

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
        escapees        = escapedBits backslashChains

escapedBitsInWord' :: Bool -> Word8 -> (Bool, Word8)
escapedBitsInWord' carry w = go 0 0 carry
  where go :: Count -> Word8 -> Bool -> (Bool, Word8)
        go n acc c = if n < 8
          then if c
            then go (n + 1) ((1 .<. n) .|. acc) False
            else go (n + 1) acc                 (((1 .<. n) .&. w) /= 0)
          else (c, acc)

escapedBitsInWord :: Bool -> Word8 -> (Bool, Word8)
escapedBitsInWord carry w = if carry
  then (escapedBitsWord1Carry DV.! fromIntegral w, escapedBitsWord1 !!! fromIntegral w)
  else (escapedBitsWord0Carry DV.! fromIntegral w, escapedBitsWord0 !!! fromIntegral w)
{-# INLINE escapedBitsInWord #-}

escapedBitsWord0 :: DVS.Vector Word8
escapedBitsWord0 = DVS.constructN 256 go
  where go :: DVS.Vector Word8 -> Word8
        go u = snd (escapedBitsInWord False (fromIntegral (DVS.length u)))
{-# NOINLINE escapedBitsWord0 #-}

escapedBitsWord0Carry :: DV.Vector Bool
escapedBitsWord0Carry = DV.constructN 256 go
  where go :: DV.Vector Bool -> Bool
        go u = fst (escapedBitsInWord False (fromIntegral (DV.length u)))
{-# NOINLINE escapedBitsWord0Carry #-}

escapedBitsWord1 :: DVS.Vector Word8
escapedBitsWord1 = DVS.constructN 256 go
  where go :: DVS.Vector Word8 -> Word8
        go u = snd (escapedBitsInWord True (fromIntegral (DVS.length u)))
{-# NOINLINE escapedBitsWord1 #-}

escapedBitsWord1Carry :: DV.Vector Bool
escapedBitsWord1Carry = DV.constructN 256 go
  where go :: DV.Vector Bool -> Bool
        go u = fst (escapedBitsInWord True (fromIntegral (DV.length u)))
{-# NOINLINE escapedBitsWord1Carry #-}

escapedBits' :: Bool -> DVS.Vector Word8 -> (Bool, DVS.Vector Word8)
escapedBits' carry v = DVS.createT $ do
  u <- DVSM.new (DVS.length v)
  go carry 0 u
  where go :: Bool -> Int -> DVSM.MVector s Word8 -> ST s (Bool, DVSM.MVector s Word8)
        go c i u = if i < DVSM.length u
          then do
            let (c', w) = escapedBitsInWord c (DVS.unsafeIndex v i)
            DVSM.write u i w
            go c' (i + 1) u
          else return (c, u)

escapedBits :: [DVS.Vector Word64] -> [DVS.Vector Word64]
escapedBits v = DVS.unsafeCast <$> snd (P.foldr go (False, []) (DVS.unsafeCast <$> v))
  where go :: DVS.Vector Word8 -> (Bool, [DVS.Vector Word8]) -> (Bool, [DVS.Vector Word8])
        go u (c, ts) = (:ts) <$> escapedBits' c u

moo :: IO ()
moo = do
  forM_ [minBound .. maxBound ::Word8] $ \w -> do
    let (w2, c) = escapedBitsInWord False w
    IO.putStrLn $ "False: " <> bitShow w <> " - " <> bitShow w2 <> " " <> show c
  IO.putStrLn ""
  forM_ [minBound .. maxBound ::Word8] $ \w -> do
    let (w2, c) = escapedBitsInWord True w
    IO.putStrLn $ "True: " <> bitShow w <> " - " <> bitShow w2 <> " " <> show c
