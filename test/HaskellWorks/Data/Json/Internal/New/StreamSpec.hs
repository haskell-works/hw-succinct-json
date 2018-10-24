{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Internal.New.StreamSpec
  ( spec
  ) where

import Data.Word
import Test.Hspec

import qualified Data.Vector.Storable                       as DVS
import qualified HaskellWorks.Data.Json.Internal.New.Stream as S

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Internal.New.StreamSpec" $ do
  it "unstream . stream" $ do
    let v = DVS.replicate 64 (0 :: Word64)
    (S.unstreamVector 64 . S.streamVector) v `shouldBe` v
  it "unstream . stream64To8 . stream" $ do
    let v = DVS.replicate 64 (0 :: Word64)
    let e = DVS.replicate 512 (0 :: Word8)
    (S.unstreamVector 512 . S.stream64To8 . S.streamVector) v `shouldBe` e
  it "unstream . stream8To64 . stream64To8 . stream" $ do
    let v = DVS.replicate 64 (0 :: Word64)
    (S.unstreamVector 64 . S.stream8To64 . S.stream64To8 . S.streamVector) v `shouldBe` v
  it "unstream . promote id . stream" $ do
    let v = DVS.replicate 64 (0 :: Word64)
    (S.unstreamVector 64 . S.promote id . S.streamVector) v `shouldBe` v
