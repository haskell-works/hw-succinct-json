{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.Function
import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Json.Cursor
import HaskellWorks.Data.Json.LightJson
import HaskellWorks.Data.Json.LoadCursor
import HaskellWorks.Data.Micro
import HaskellWorks.Data.MQuery

import qualified Data.ByteString      as BS
import qualified Data.DList           as DL
import qualified Data.Vector.Storable as DVS

readJson :: String -> IO (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
readJson path = do
  bs <- BS.readFile path
  print ("Read file" :: String)
  let !cursor = fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
  print ("Created cursor" :: String)
  return cursor

main :: IO ()
main = do
  !cursor <- loadJsonWithCsPoppyIndex "../data/78mb.json"

  let !json = lightJsonAt cursor
  let q = MQuery (DL.singleton json)

  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString) & count
