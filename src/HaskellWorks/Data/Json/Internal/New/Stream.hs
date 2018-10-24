module HaskellWorks.Data.Json.Internal.New.Stream
  ( evens
  , odds

  , streamLazyByteString
  , streamByteStrings
  , streamByteString
  , streamVector
  , streamVectors
  , unstreamVector
  , unstreamVectors
  , promote

  , stream8To64
  , stream64To8
  ) where

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Bits.Pdep
import Data.Maybe
import Data.Vector.Fusion.Bundle.Size            (upperBound)
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Streams.Size
import HaskellWorks.Data.Streams.Stream

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Unsafe           as BSU
import qualified Data.Vector.Storable             as DVS
import qualified Data.Vector.Storable.Mutable     as DVSM
import qualified HaskellWorks.Data.Streams.Stream as S

odds :: Stream Word64 -> Stream Word64
odds = alternates 0

evens :: Stream Word64 -> Stream Word64
evens = alternates 1

alternates :: Count -> Stream Word64 -> Stream Word64
alternates = transcribe go
  where go :: Count -> Word64 -> (Word64, Count)
        go carry w = (pdep (0x5555555555555555 .<. (carry .&. 1)) w, popCount1 w + carry)

streamLazyByteString :: LBS.ByteString -> Stream Word8
streamLazyByteString = streamByteStrings . LBS.toChunks
{-# INLINE streamLazyByteString #-}

streamByteStrings :: [BS.ByteString] -> Stream Word8
streamByteStrings bss = Stream step (BS.empty, bss) Unknown
  where step :: (BS.ByteString, [BS.ByteString]) -> Step (BS.ByteString, [BS.ByteString]) Word8
        step (cs, css) = if BS.null cs
          then case css of
            ds:dss -> Skip (ds, dss)
            []     -> Done
          else Yield (BSU.unsafeHead cs) (BSU.unsafeTail cs, css)
        {-# INLINE step #-}
{-# INLINE streamByteStrings #-}

streamByteString :: BS.ByteString -> Stream Word8
streamByteString bs = Stream step bs Unknown
  where step :: BS.ByteString -> Step BS.ByteString Word8
        step cs = if BS.null cs
          then Done
          else Yield (BSU.unsafeHead cs) (BSU.unsafeTail cs)
        {-# INLINE step #-}
{-# INLINE streamByteString #-}

streamVectors :: [DVS.Vector Word64] -> Stream Word64
streamVectors vs = Stream step (vs, 0) Unknown
  where step :: ([DVS.Vector Word64], Int) -> Step ([DVS.Vector Word64], Int) Word64
        step ([], _) = Done
        step (us@(t:ts), i) = if i < DVS.length t
          then Yield (t DVS.! i) (us, i + 1)
          else Skip (ts, 0)
        {-# INLINE step #-}
{-# INLINE streamVectors #-}

streamVector :: DVS.Vector Word64 -> Stream Word64
streamVector v = Stream step 0 Unknown
  where step i = if i > DVS.length v then Done else Yield (v DVS.! i) (i + 1)
        {-# INLINE step #-}
{-# INLINE streamVector #-}

-- TODO Replace maxSize with hintSize so vector can grow
unstreamVector :: DVSM.Storable a => Int -> Stream a -> DVS.Vector a
unstreamVector maxSize (Stream step s size) = DVS.create $ do
  mv <- DVSM.unsafeNew allocSize
  go 0 step s mv
  where allocSize = min maxSize (fromMaybe maxBound (upperBound size))
        go :: DVSM.Storable a => Int -> (t -> Step t a) -> t -> DVSM.MVector s a -> ST s (DVSM.MVector s a)
        go i step' s' mv = if i >= DVSM.length mv
          then return mv
          else case step' s' of
            Yield a s'' -> do
              DVSM.unsafeWrite mv i a
              go (i + 1) step' s'' mv
            Skip s'' -> go i step' s'' mv
            Done -> return (DVSM.take i mv)
{-# INLINE unstreamVector #-}

unstreamVectors :: DVSM.Storable a => Int -> Stream a -> [DVS.Vector a]
unstreamVectors chunkSize (Stream step s size) = DVS.createT $ do
  mv <- DVSM.unsafeNew chunkSize
  go 0 step s mv
  where go i step' s' mv = if i >= DVSM.length mv
          then do
            mus <- unsafeInterleaveST $ do
              newMv <- DVSM.unsafeNew chunkSize
              go 0 step' s' newMv
            return (mv:mus)
          else case step' s' of
            Yield a s'' -> do
              DVSM.unsafeWrite mv i a
              go (i + 1) step' s'' mv
            Skip s'' -> go i step' s'' mv
            Done -> return [DVSM.take i mv | i > 0]
{-# INLINE unstreamVectors #-}

word64ToStream8 :: Word64 -> Stream Word8
word64ToStream8 w = S.Stream step 0 (Exact 8)
  where step :: Count -> Step Count Word8
        step n = if n < 8
          then Yield (fromIntegral ((w .>. (8 * n)) .&. 0xff)) (n + 1)
          else Done
{-# INLINE word64ToStream8 #-}

stream64To8 :: Stream Word64 -> Stream Word8
stream64To8 = S.concatMap word64ToStream8
{-# INLINE stream64To8 #-}

stream8To64 :: Stream Word8 -> Stream Word64
stream8To64 (Stream stepA stateA _) = Stream (stepB stepA) (stateA, 0, 0) Unknown
  where stepB :: (s -> Step s Word8) -> (s, Int, Word64) -> Step (s, Int, Word64) Word64
        stepB step (stateB, i, w) = case step stateB of
          Yield w' stateC -> if i < 7
            then Skip  (stateC, i + 1, w .|. (fromIntegral w' .<. (fromIntegral i * 8)))
            else Yield (w .|. (fromIntegral w' .<. (fromIntegral i * 8))) (stateC, 0, 0)
          Skip stateC    -> Skip (stateC, i, w)
          Done      -> if i > 0
            then Yield w (stateB, 0, 0)
            else Done
{-# INLINE stream8To64 #-}

promote :: (Stream Word8 -> Stream Word8) -> Stream Word64 -> Stream Word64
promote f = stream8To64 . f . stream64To8
{-# INLINE promote #-}

