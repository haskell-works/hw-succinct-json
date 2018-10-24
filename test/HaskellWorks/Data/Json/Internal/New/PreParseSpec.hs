{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Internal.New.PreParseSpec
  ( spec
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitShow
import Test.Hspec

import qualified Data.ByteString                               as BS
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import qualified Data.Vector.Fixed                             as DVF
import qualified Data.Vector.Storable                          as DVS
import qualified HaskellWorks.Data.Json.Internal.New.PreParseB as PPB
import qualified HaskellWorks.Data.Json.Internal.New.Stream    as S

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

interestBitsOf :: BS.ByteString -> DVS.Vector Word64
interestBitsOf bs = PPB.buildSemiIndex (BS.length bs) (S.streamByteString bs) DVF.! 0

bitShowIbOf :: BS.ByteString -> String
bitShowIbOf = bitShow . interestBitsOf

unpackDecodeUtf8 :: BS.ByteString -> String
unpackDecodeUtf8 = T.unpack . T.decodeUtf8

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.Cursor.InterestBitsSpec" $ do
  describe "ET.decodeUtf8 valuating interest bits" $ do
    let ex = ""            in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` ""
    let ex = "  \n \r \t " in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "1234 "       in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "false "      in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "true "       in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "\"hello\" "  in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "\"\\\"\" "   in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "{ "          in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "} "          in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "[ "          in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "] "          in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = ": "          in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = ", "          in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "{{}}"        in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "11110000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = " { { } } "   in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "01010101 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "[\"\\\\]\"]" in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "10000010 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    let ex = "[\"{[]}\"]"  in it (unpackDecodeUtf8 ex) $ bitShowIbOf ex `shouldBe` "10000001 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
