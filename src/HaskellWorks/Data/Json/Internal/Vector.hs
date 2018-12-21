{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Internal.Vector where

import Control.Monad.ST
import Data.Traversable
import Data.Word
import HaskellWorks.Data.Vector.AsVector8

import qualified Data.ByteString              as BS
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM

data Vec2 a = Vec2
  { vec2A :: a
  , vec2B :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

unroll2N :: Int -> [(BS.ByteString, BS.ByteString)] -> Vec2 (DVS.Vector Word64)
unroll2N n _ = DVS.createT $ do
  ibv <- DVSM.new (n * 8)
  bpv <- DVSM.new (n * 8)
  go ibv 0 bpv 0 []
  where go :: ()
          => DVSM.MVector s Word8
          -> Int
          -> DVSM.MVector s Word8
          -> Int
          -> [(BS.ByteString, BS.ByteString)]
          -> ST s (Vec2 (DVSM.MVector s Word64))
        go ibv ibi bpv bpi ((ib, bp):as) = do
          let ibu = asVector8 ib
          let bpu = asVector8 bp
          DVS.unsafeCopy (DVSM.drop ibi ibv) ibu
          DVS.unsafeCopy (DVSM.drop bpi bpv) bpu
          return undefined
          -- let vib64 = DVSM.unsafeCast ibv
          -- let vbp64 = DVSM.unsafeCast bpv
          -- return (Vec2 vib64 vbp64)
