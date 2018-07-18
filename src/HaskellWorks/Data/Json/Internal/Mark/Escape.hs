module HaskellWorks.Data.Json.Internal.Mark.Escape
  ( markEscapedBits
  ) where

import Control.Monad
import Control.Monad.ST               (ST)
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import Prelude                        hiding (foldr)

import qualified Data.Vector                  as DV
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM
import qualified Prelude                      as P

escapedBitsInWord' :: Bool -> Word8 -> (Bool, Word8)
escapedBitsInWord' carry w = go 0 0 carry
  where go :: Count -> Word8 -> Bool -> (Bool, Word8)
        go n acc c = if n < 8
          then if c
            then go (n + 1) ((1 .<. n) .|. acc) False
            else go (n + 1) acc                 (((1 .<. n) .&. w) /= 0)
          else (c, acc)
{-# INLINE escapedBitsInWord' #-}

escapedBitsInWord :: Bool -> Word8 -> (Bool, Word8)
escapedBitsInWord carry w = if carry
  then (escapedBitsWord1Carry DV.! fromIntegral w, escapedBitsWord1 !!! fromIntegral w)
  else (escapedBitsWord0Carry DV.! fromIntegral w, escapedBitsWord0 !!! fromIntegral w)
{-# INLINE escapedBitsInWord #-}

escapedBitsWord0 :: DVS.Vector Word8
escapedBitsWord0 = DVS.constructN 256 go
  where go :: DVS.Vector Word8 -> Word8
        go u = snd (escapedBitsInWord' False (fromIntegral (DVS.length u)))
{-# NOINLINE escapedBitsWord0 #-}

escapedBitsWord0Carry :: DV.Vector Bool
escapedBitsWord0Carry = DV.constructN 256 go
  where go :: DV.Vector Bool -> Bool
        go u = fst (escapedBitsInWord' False (fromIntegral (DV.length u)))
{-# NOINLINE escapedBitsWord0Carry #-}

escapedBitsWord1 :: DVS.Vector Word8
escapedBitsWord1 = DVS.constructN 256 go
  where go :: DVS.Vector Word8 -> Word8
        go u = snd (escapedBitsInWord' True (fromIntegral (DVS.length u)))
{-# NOINLINE escapedBitsWord1 #-}

escapedBitsWord1Carry :: DV.Vector Bool
escapedBitsWord1Carry = DV.constructN 256 go
  where go :: DV.Vector Bool -> Bool
        go u = fst (escapedBitsInWord' True (fromIntegral (DV.length u)))
{-# NOINLINE escapedBitsWord1Carry #-}

markEscapedBits' :: Bool -> DVS.Vector Word8 -> (Bool, DVS.Vector Word8)
markEscapedBits' carry v = DVS.createT $ do
  u <- DVSM.new (DVS.length v)
  go carry 0 u
  where go :: Bool -> Int -> DVSM.MVector s Word8 -> ST s (Bool, DVSM.MVector s Word8)
        go c i u = if i < DVSM.length u
          then do
            let (c', w) = escapedBitsInWord c (DVS.unsafeIndex v i)
            DVSM.write u i w
            go c' (i + 1) u
          else return (c, u)
{-# INLINE markEscapedBits' #-}

markEscapedBits :: [DVS.Vector Word64] -> [DVS.Vector Word64]
markEscapedBits v = DVS.unsafeCast <$> snd (P.foldr go (False, []) (DVS.unsafeCast <$> v))
  where go :: DVS.Vector Word8 -> (Bool, [DVS.Vector Word8]) -> (Bool, [DVS.Vector Word8])
        go u (c, ts) = (:ts) <$> markEscapedBits' c u
{-# INLINE markEscapedBits #-}
