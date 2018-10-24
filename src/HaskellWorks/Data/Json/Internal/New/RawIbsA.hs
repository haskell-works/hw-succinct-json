{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Internal.New.RawIbsA where

import Data.Bits.Pdep
import Data.Word
import HaskellWorks.Data.Bits.BitWise      hiding (comp)
import HaskellWorks.Data.Streams.Stream
import HaskellWorks.Data.Vector.AsVector64

import qualified Data.Bits                                    as DB
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.Vector                                  as DV
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.Bits.BitWise               as BW
import qualified HaskellWorks.Data.ByteString                 as BS
import qualified HaskellWorks.Data.Json.Internal.New.Char     as C
import qualified HaskellWorks.Data.Json.Internal.New.Internal as I
import qualified HaskellWorks.Data.Json.Internal.New.Stream   as S
import qualified HaskellWorks.Data.Simd.Comparison            as SIMD
import qualified HaskellWorks.Data.Streams.Stream             as S
import qualified HaskellWorks.Data.Streams.Stream.Ops         as S

extractRawInterestBits :: LBS.ByteString -> (Stream Word64, Stream Word64)
extractRawInterestBits lbs = (maskedIbs, paddedParens)
  where bss :: [DVS.Vector Word64]
        bss = asVector64 <$> BS.resegmentPadded 512 (LBS.toChunks lbs)
        rawIbsQuotes            = S.streamVectors $ SIMD.cmpEqWord8s C.doubleQuote  <$> bss
        rawIbsBackSlashes       = S.streamVectors $ SIMD.cmpEqWord8s C.backSlash    <$> bss
        rawIbsOpenBraces        = S.streamVectors $ SIMD.cmpEqWord8s C.openBrace    <$> bss
        rawIbsCloseBraces       = S.streamVectors $ SIMD.cmpEqWord8s C.closeBrace   <$> bss
        rawIbsOpenBrackets      = S.streamVectors $ SIMD.cmpEqWord8s C.openBracket  <$> bss
        rawIbsCloseBrackets     = S.streamVectors $ SIMD.cmpEqWord8s C.closeBracket <$> bss
        rawIbsCommas            = S.streamVectors $ SIMD.cmpEqWord8s C.comma        <$> bss
        rawIbsColon             = S.streamVectors $ SIMD.cmpEqWord8s C.colon        <$> bss
        rawEscapeMask           = escape rawIbsBackSlashes -- TODO For some reason this produces half the size
        quotes                  = S.bitwiseAnd rawIbsQuotes rawEscapeMask
        openQuotes              = S.odds  quotes
        closeQuotes             = S.evens quotes
        quoteMask               = S.add (S.comp closeQuotes) openQuotes
        maskedIbsOpens          = S.bitwiseAnd quoteMask (S.bitwiseOr rawIbsOpenBraces   rawIbsOpenBrackets)
        maskedIbsCloses         = S.bitwiseAnd quoteMask (S.bitwiseOr rawIbsCloseBraces  rawIbsCloseBrackets)
        maskedIbsSeps           = S.bitwiseAnd quoteMask (S.bitwiseOr rawIbsCommas       rawIbsColon)
        maskedIbs               = maskedIbsOpens `S.bitwiseOr` maskedIbsCloses `S.bitwiseOr` maskedIbsSeps
        paddedParensOpenBraces  = map1To11 maskedIbsOpens
        paddedParensDelims      = map1To11 maskedIbsOpens
        paddedParens            = S.bitwiseOr paddedParensOpenBraces paddedParensDelims


map1To01 :: Stream Word64 -> Stream Word64
map1To01 = S.dupMap lo hi
  where lo w = pdep w 0xaaaaaaaaaaaaaaaa
        hi w = let w' = w .>. 32 in pdep w' 0xaaaaaaaaaaaaaaaa

map1To11 :: Stream Word64 -> Stream Word64
map1To11 = S.dupMap lo hi
  where lo w = pdep w 0x5555555555555555 .|. pdep w 0xaaaaaaaaaaaaaaaa
        hi w = let w' = w .>. 32 in pdep w' 0x5555555555555555 .|. pdep w' 0xaaaaaaaaaaaaaaaa

escape :: Stream Word64 -> Stream Word64
escape = S.promote escape'

escape' :: Stream Word8 -> Stream Word8
escape' = S.transcribe go (0 :: Int)
  where go :: Int -> Word8 -> (Word8, Int)
        go lastTrailingOnes w = (m, nextTrailingZeros)
          where m = I.escapeMask DV.! (lastTrailingOnes `mod` 2) DVS.! fromIntegral w
                nextTrailingZeros = DB.countLeadingZeros (BW.comp w)
{-# INLINE escape' #-}

(#|#) :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
(#|#) (as:ass) (bs:bss) = DVS.zipWith (.|.) as bs:(ass #|# bss)
(#|#) _         _       = []
{-# INLINE (#|#) #-}

(#&#) :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
(#&#) (as:ass) (bs:bss) = DVS.zipWith (.&.) as bs:(ass #&# bss)
(#&#) _         _       = []
{-# INLINE (#&#) #-}
