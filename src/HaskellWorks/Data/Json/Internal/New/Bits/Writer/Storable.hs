{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Internal.New.Bits.Writer.Storable where

import Control.Monad.ST
import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Vector.Storable.Mutable as DVSM

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

data Writer s = Writer
  { vector   :: !(DVSM.MVector s Word64)
  , position :: !Int
  }

full :: Writer s -> Bool
full (Writer v p) = p * 64 >= DVSM.length v
{-# INLINE full #-}

newWriter :: Int -> ST s (Writer s)
newWriter size = do
  v <- DVSM.new size
  return $ Writer v 0

unsafeWriteBit :: Writer s -> Word64 -> ST s (Writer s)
unsafeWriteBit b w = do
  let v = vector   b -- vector
  let p = position b -- position
  let i = p .>. 6    -- index into vector
  let o = p .&. 0x3f -- offset within a word
  e <- DVSM.unsafeRead v i
  DVSM.unsafeWrite v i (((w .&. 1) .<. fromIntegral o) .|. e)
  return b { position = p + 1 }
{-# INLINE unsafeWriteBit #-}

unsafeWriteLoBits :: Writer s -> Int -> Word64 -> ST s (Writer s)
unsafeWriteLoBits b c w = do
  let u = w .&. ((1 .<. fromIntegral c) - 1)
  let v = vector   b -- vector
  let p = position b -- position
  let i = p .>. 6    -- index into vector
  let o = p .&. 0x3f -- offset within a word
  lo <- DVSM.unsafeRead v i
  DVSM.unsafeWrite v i $ lo .|. (u .<. fromIntegral o)
  return b { position = p + c }
{-# INLINE unsafeWriteLoBits #-}

unsafeWriteBits :: Writer s -> Int -> Word64 -> ST s (Writer s)
unsafeWriteBits b c w = do
  let u = w .&. ((1 .<. fromIntegral c) - 1)  -- masked word
  let v = vector   b                          -- vector
  let p = position b                          -- position
  let i = p .>. 6                             -- index into vector for lo part
  let j = i + 1                               -- index into vector for hi part
  let o = p .&. 0x3f                          -- offset within a word
  lo <- DVSM.unsafeRead v i
  DVSM.unsafeWrite v i $ lo .|. (u .<. fromIntegral o)
  hi <- DVSM.unsafeRead v j
  DVSM.unsafeWrite v j $ hi .|. (u .<. fromIntegral (64 - o))
  return b { position = p + c }
{-# INLINE unsafeWriteBits #-}

written :: Writer s -> DVSM.MVector s Word64
written w = DVSM.take ((position w + 63) `div` 64) (vector w)
{-# INLINE written #-}
