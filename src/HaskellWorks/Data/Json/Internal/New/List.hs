module HaskellWorks.Data.Json.Internal.New.List where

import Data.Word
import HaskellWorks.Data.Json.Internal.New.Internal
import HaskellWorks.Data.Vector.AsVector8

import qualified Data.Bits                      as DB
import qualified Data.Vector                    as DV
import qualified Data.Vector.Storable           as DVS
import qualified HaskellWorks.Data.Bits.BitWise as BW

comp :: [DVS.Vector Word64] -> [DVS.Vector Word64]
comp = fmap (DVS.map BW.comp)

escape :: [DVS.Vector Word64] -> [DVS.Vector Word64]
escape = fmap DVS.unsafeCast . go 0 . fmap asVector8
  where go :: Int -> [DVS.Vector Word8] -> [DVS.Vector Word8]
        go _ []     = []
        go c (v:vs) = let (u, d) = escape' c v in u:go d vs

escape' :: Int -> DVS.Vector Word8 -> (DVS.Vector Word8, Int)
escape' c v = (u, e)
  where vLen = DVS.length v
        u :: DVS.Vector Word8
        u = DVS.constructN vLen go
        go :: DVS.Vector Word8 -> Word8
        go t = escapeMask DV.! (d `mod` 2) DVS.! fromIntegral (v DVS.! fromIntegral ti)
          where ti = DVS.length t
                d = if ti == 0
                  then c
                  else DB.countLeadingZeros (BW.comp (v DVS.! (ti - 1)))
        e = if DVS.length v > 0
              then DB.countLeadingZeros (BW.comp (v DVS.! (vLen - 1)))
              else c
