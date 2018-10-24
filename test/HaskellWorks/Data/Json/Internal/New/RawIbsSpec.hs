{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Internal.New.RawIbsSpec
  ( spec
  , bitShowIbOf
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitShow
import Test.Hspec

import qualified Data.ByteString                               as BS hiding (putStrLn)
import qualified Data.ByteString.Lazy                          as LBS
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import qualified Data.Vector.Fixed                             as DVF
import qualified Data.Vector.Storable                          as DVS
import qualified HaskellWorks.Data.Json.Internal.New.PreParseB as PPB
import qualified HaskellWorks.Data.Json.Internal.New.RawIbsA   as RIBA
import qualified HaskellWorks.Data.Json.Internal.New.Stream    as S

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

interestBitsOf :: BS.ByteString -> DVS.Vector Word64
interestBitsOf bs = PPB.buildSemiIndex (BS.length bs) (S.streamByteString bs) DVF.! 0

rawInterestBitsOf :: BS.ByteString -> DVS.Vector Word64
rawInterestBitsOf bs = S.unstreamVector ((BS.length bs + 63) `div` 64) (fst (RIBA.extractRawInterestBits (LBS.fromStrict bs)))

bitShowIbOf :: BS.ByteString -> String
bitShowIbOf = bitShow . interestBitsOf

bitShowRibOf :: BS.ByteString -> String
bitShowRibOf = bitShow . rawInterestBitsOf

unpackDecodeUtf8 :: BS.ByteString -> String
unpackDecodeUtf8 = T.unpack . T.decodeUtf8

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Internal.New.RawIbsSpec" $ do
  describe "ET.decodeUtf8 valuating interest bits" $ do
    let ex = ""                       in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` ""
    let ex = "  \n \r \t "            in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "1234 "                  in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "false "                 in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "true "                  in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "\"hello\" "             in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "\"\\\"\" "              in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "{ "                     in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "} "                     in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "[ "                     in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "] "                     in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = ": "                     in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = ", "                     in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "{{}}"                   in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "11110000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = " { { } } "              in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "01010101 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "[\"\\\\]\"]"            in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "10000010 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "[\"{[]}\"]"             in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "10000001 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "[\"\\\\\\\"\"]"         in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "10000001 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = " [\"\\\\\\\"\"]"        in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "01000000 10000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "  [\"\\\\\\\"\"]"       in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00100000 01000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "   [\"\\\\\\\"\"]"      in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00010000 00100000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "    [\"\\\\\\\"\"]"     in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00001000 00010000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "     [\"\\\\\\\"\"]"    in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00000100 00001000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "      [\"\\\\\\\"\"]"   in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00000010 00000100 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "       [\"\\\\\\\"\"]"  in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00000001 00000010 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "        [\"\\\\\\\"\"]" in it (unpackDecodeUtf8 ex) $ bitShowRibOf ex `shouldBe` "00000000 10000001 00000000 00000000 00000000 00000000 00000000 00000000"
