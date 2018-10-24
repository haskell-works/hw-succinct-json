{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Internal.New.RawIbsE
  ( extractRawInterestBits
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise      hiding (comp)
import HaskellWorks.Data.Vector.AsVector64

import qualified Data.ByteString.Lazy                     as LBS
import qualified Data.Vector                              as DV
import qualified Data.Vector.Storable                     as DVS
import qualified HaskellWorks.Data.ByteString             as BS
import qualified HaskellWorks.Data.Json.Internal.New.Char as C
import qualified HaskellWorks.Data.Simd.Comparison        as SIMD
import qualified HaskellWorks.Data.Simd.Comparison.Avx2   as SIMD (cmpEqWord8sPara)

type V64 = DVS.Vector Word64

unzip8 :: [(a, b, c, d, e, f, g, h)] -> ([a], [b], [c], [d], [e], [f], [g], [h])
unzip8 ((a, b, c, d, e, f, g, h):xs) = (a:as, b:bs, c:cs, d:ds, e:es, f:fs, g:gs, h:hs)
  where (as, bs, cs, ds, es, fs, gs, hs) = unzip8 xs
unzip8 [] = ([], [], [], [], [], [], [], [])

cmpEqWord8sPara :: ()
  => [V64]
  -> ([V64], [V64], [V64], [V64], [V64], [V64], [V64], [V64])
cmpEqWord8sPara = unzip8 . fmap go
  where w8s = DVS.fromList
          [ C.doubleQuote , C.backSlash   , C.openBrace , C.closeBrace
          , C.openBracket , C.closeBracket, C.comma     , C.colon
          ]
        go :: ()
          => V64
          -> (V64, V64, V64, V64, V64, V64, V64, V64)
        go u = let rs = SIMD.cmpEqWord8sPara w8s u in
          ( rs DV.! 0, rs DV.! 1, rs DV.! 2, rs DV.! 3
          , rs DV.! 4, rs DV.! 5, rs DV.! 6, rs DV.! 7
          )


extractRawInterestBits :: LBS.ByteString -> [V64]
extractRawInterestBits lbs = rawIbs
  where bss :: [V64]
        bss = asVector64 <$> BS.resegmentPadded 512 (LBS.toChunks lbs)
        (   rawIbsQuotes
          , rawIbsBackSlashes
          , rawIbsOpenBraces
          , rawIbsCloseBraces
          , rawIbsOpenBrackets
          , rawIbsCloseBrackets
          , rawIbsCommas
          , rawIbsColon)         = cmpEqWord8sPara bss
        rawIbs              =
          rawIbsQuotes        #|#
          rawIbsBackSlashes   #|#
          rawIbsOpenBraces    #|#
          rawIbsCloseBraces   #|#
          rawIbsOpenBrackets  #|#
          rawIbsCloseBrackets #|#
          rawIbsCommas        #|#
          rawIbsColon

(#|#) :: [V64] -> [V64] -> [V64]
(#|#) (as:ass) (bs:bss) = DVS.zipWith (.|.) as bs:(ass #|# bss)
(#|#) _         _       = []
{-# INLINE (#|#) #-}
