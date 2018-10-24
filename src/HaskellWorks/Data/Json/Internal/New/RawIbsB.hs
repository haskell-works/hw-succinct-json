{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Internal.New.RawIbsB where

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
import qualified HaskellWorks.Data.Json.Internal.New.Internal as I
import qualified HaskellWorks.Data.Json.Internal.New.Stream   as S
import qualified HaskellWorks.Data.Json.Internal.Word8        as W8
import qualified HaskellWorks.Data.Simd.Comparison            as SIMD
import qualified HaskellWorks.Data.Streams.Stream             as S
import qualified HaskellWorks.Data.Streams.Stream.Ops         as S

extractRawInterestBits :: LBS.ByteString -> (Stream Word64, Stream Word64)
extractRawInterestBits lbs = (maskedIbs, paddedParens)
  where bss :: [DVS.Vector Word64]
        bss = asVector64 <$> BS.resegmentPadded 512 (LBS.toChunks lbs)
        rawIbsQuotes            = S.streamVectors $ SIMD.cmpEqWord8s W8.doubleQuote   <$> bss
        rawIbsBackSlashes       = S.streamVectors $ SIMD.cmpEqWord8s W8.backSlash     <$> bss
        rawIbsOpenBraces        = S.streamVectors $ SIMD.cmpEqWord8s W8.openBrace     <$> bss
        rawIbsCloseBraces       = S.streamVectors $ SIMD.cmpEqWord8s W8.closeBrace    <$> bss
        rawIbsOpenBrackets      = S.streamVectors $ SIMD.cmpEqWord8s W8.openBracket   <$> bss
        rawIbsCloseBrackets     = S.streamVectors $ SIMD.cmpEqWord8s W8.closeBracket  <$> bss
        rawIbsCommas            = S.streamVectors $ SIMD.cmpEqWord8s W8.comma         <$> bss
        rawIbsColon             = S.streamVectors $ SIMD.cmpEqWord8s W8.colon         <$> bss
        rawEscapeMask           = escape rawIbsBackSlashes
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
escape = S.transcribe go (0 :: Int)
  where go :: Int -> Word64 -> (Word64, Int)
        go lastTrailingOnes w = (m, c8)
          where w0 = fromIntegral $ w        :: Word8
                w1 = fromIntegral $ w .>. 8  :: Word8
                w2 = fromIntegral $ w .>. 16 :: Word8
                w3 = fromIntegral $ w .>. 24 :: Word8
                w4 = fromIntegral $ w .>. 32 :: Word8
                w5 = fromIntegral $ w .>. 40 :: Word8
                w6 = fromIntegral $ w .>. 48 :: Word8
                w7 = fromIntegral $ w .>. 56 :: Word8
                c0 = lastTrailingOnes
                m0 = I.escapeMask DV.! (c0 `mod` 2) DVS.! fromIntegral w0
                c1 = DB.countLeadingZeros (BW.comp w0)
                m1 = I.escapeMask DV.! (c1 `mod` 2) DVS.! fromIntegral w1
                c2 = DB.countLeadingZeros (BW.comp w1)
                m2 = I.escapeMask DV.! (c2 `mod` 2) DVS.! fromIntegral w2
                c3 = DB.countLeadingZeros (BW.comp w2)
                m3 = I.escapeMask DV.! (c3 `mod` 2) DVS.! fromIntegral w3
                c4 = DB.countLeadingZeros (BW.comp w3)
                m4 = I.escapeMask DV.! (c4 `mod` 2) DVS.! fromIntegral w4
                c5 = DB.countLeadingZeros (BW.comp w4)
                m5 = I.escapeMask DV.! (c5 `mod` 2) DVS.! fromIntegral w5
                c6 = DB.countLeadingZeros (BW.comp w5)
                m6 = I.escapeMask DV.! (c6 `mod` 2) DVS.! fromIntegral w6
                c7 = DB.countLeadingZeros (BW.comp w6)
                m7 = I.escapeMask DV.! (c7 `mod` 2) DVS.! fromIntegral w7
                c8 = DB.countLeadingZeros (BW.comp w7)
                m  =  (fromIntegral m7 .<. 56) .|.
                      (fromIntegral m6 .<. 48) .|.
                      (fromIntegral m5 .<. 40) .|.
                      (fromIntegral m4 .<. 32) .|.
                      (fromIntegral m3 .<. 24) .|.
                      (fromIntegral m2 .<. 16) .|.
                      (fromIntegral m1 .<. 8 ) .|.
                       fromIntegral m0
{-# INLINE escape #-}

(#|#) :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
(#|#) (as:ass) (bs:bss) = DVS.zipWith (.|.) as bs:(ass #|# bss)
(#|#) _         _       = []
{-# INLINE (#|#) #-}

(#&#) :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
(#&#) (as:ass) (bs:bss) = DVS.zipWith (.&.) as bs:(ass #&# bss)
(#&#) _         _       = []
{-# INLINE (#&#) #-}
