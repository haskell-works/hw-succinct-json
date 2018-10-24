{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Internal.New.RawIbsD where

import Data.Word
import HaskellWorks.Data.Bits.BitWise      hiding (comp)
import HaskellWorks.Data.Vector.AsVector64

import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.Vector.Storable                  as DVS
import qualified HaskellWorks.Data.ByteString          as BS
import qualified HaskellWorks.Data.Json.Internal.Word8 as W8
import qualified HaskellWorks.Data.Simd.Comparison     as SIMD

extractRawInterestBits :: LBS.ByteString -> [DVS.Vector Word64]
extractRawInterestBits lbs = rawIbs
  where bss :: [DVS.Vector Word64]
        bss = asVector64 <$> BS.resegmentPadded 512 (LBS.toChunks lbs)
        rawIbsQuotes        = SIMD.cmpEqWord8s W8.doubleQuote  <$> bss
        rawIbsBackSlashes   = SIMD.cmpEqWord8s W8.backSlash    <$> bss
        rawIbsOpenBraces    = SIMD.cmpEqWord8s W8.openBrace    <$> bss
        rawIbsCloseBraces   = SIMD.cmpEqWord8s W8.closeBrace   <$> bss
        rawIbsOpenBrackets  = SIMD.cmpEqWord8s W8.openBracket  <$> bss
        rawIbsCloseBrackets = SIMD.cmpEqWord8s W8.closeBracket <$> bss
        rawIbsCommas        = SIMD.cmpEqWord8s W8.comma        <$> bss
        rawIbsColon         = SIMD.cmpEqWord8s W8.colon        <$> bss
        rawIbs              =
          rawIbsQuotes        #|#
          rawIbsBackSlashes   #|#
          rawIbsOpenBraces    #|#
          rawIbsCloseBraces   #|#
          rawIbsOpenBrackets  #|#
          rawIbsCloseBrackets #|#
          rawIbsCommas        #|#
          rawIbsColon

(#|#) :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
(#|#) (as:ass) (bs:bss) = DVS.zipWith (.|.) as bs:(ass #|# bss)
(#|#) _         _       = []
{-# INLINE (#|#) #-}
