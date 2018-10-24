{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Internal.New.RawIbsD where

import Data.Word
import HaskellWorks.Data.Bits.BitWise      hiding (comp)
import HaskellWorks.Data.Vector.AsVector64

import qualified Data.ByteString.Lazy                     as LBS
import qualified Data.Vector.Storable                     as DVS
import qualified HaskellWorks.Data.ByteString             as BS
import qualified HaskellWorks.Data.Json.Internal.New.Char as C
import qualified HaskellWorks.Data.Simd.Comparison        as SIMD

extractRawInterestBits :: LBS.ByteString -> [DVS.Vector Word64]
extractRawInterestBits lbs = rawIbs
  where bss :: [DVS.Vector Word64]
        bss = asVector64 <$> BS.resegmentPadded 512 (LBS.toChunks lbs)
        rawIbsQuotes        = SIMD.cmpEqWord8s C.doubleQuote  <$> bss
        rawIbsBackSlashes   = SIMD.cmpEqWord8s C.backSlash    <$> bss
        rawIbsOpenBraces    = SIMD.cmpEqWord8s C.openBrace    <$> bss
        rawIbsCloseBraces   = SIMD.cmpEqWord8s C.closeBrace   <$> bss
        rawIbsOpenBrackets  = SIMD.cmpEqWord8s C.openBracket  <$> bss
        rawIbsCloseBrackets = SIMD.cmpEqWord8s C.closeBracket <$> bss
        rawIbsCommas        = SIMD.cmpEqWord8s C.comma        <$> bss
        rawIbsColon         = SIMD.cmpEqWord8s C.colon        <$> bss
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
