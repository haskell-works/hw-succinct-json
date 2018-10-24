{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf   #-}

module HaskellWorks.Data.Json.Internal.New.PreParseB
  ( buildSemiIndex
  ) where

import Control.Monad.ST
import Data.Vector.Fixed
import Data.Vector.Fixed.Boxed
import Data.Word
import HaskellWorks.Data.Streams.Stream (Step (..), Stream (Stream))

import qualified Data.Vector.Storable                   as DVS
import qualified HaskellWorks.Data.Bits.Writer.Storable as W
import qualified HaskellWorks.Data.Json.Internal.Word8  as W8

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

data Context = InJson | InString | InEscape deriving (Eq, Show)

buildSemiIndex :: Int -> Stream Word8 -> Vec2 (DVS.Vector Word64)
buildSemiIndex hintSize (Stream step state _) = DVS.createT $ do
  let len = hintSize
  ib <- W.newWriter len
  bp <- W.newWriter (len * 2)
  go step state InJson ib bp
  where go :: (s -> Step s Word8) -> s -> Context -> W.Writer t -> W.Writer t -> ST t (Vec2 (DVS.MVector t Word64))
        go step' state' context ib bp = case step' state' of
          Yield c state'' -> do
            case context of
              InJson -> if
                | c == W8.openBracket || c == W8.openBrace -> do
                  W.unsafeWriteBits bp 2 0x3
                  W.unsafeWriteBit ib 1
                  go step' state'' InJson ib bp
                | c == W8.closeBracket || c == W8.closeBrace -> do
                  W.unsafeWriteBits bp 2 0x0
                  W.unsafeWriteBit ib 1
                  go step' state'' InJson ib bp
                | c == W8.comma || c == W8.colon -> do
                  W.unsafeWriteBits bp 2 0x2
                  W.unsafeWriteBit ib 1
                  go step' state'' InJson ib bp
                | c == W8.doubleQuote -> do
                  W.unsafeWriteBit ib 0
                  go step' state'' InString ib bp
                | otherwise -> do
                  W.unsafeWriteBit ib 0
                  go step' state'' InJson ib bp
              InString -> do
                W.unsafeWriteBit ib 0
                let newContext = if
                      | c == W8.doubleQuote  -> InJson
                      | c == W8.backSlash    -> InEscape
                      | otherwise           -> InString
                go step' state'' newContext ib bp
              InEscape -> do
                W.unsafeWriteBit ib 0
                go step' state'' InString ib bp
          Done -> do
            ibv <- W.written ib
            bpv <- W.written bp
            return (mk2 ibv bpv)
          Skip state'' -> go step' state'' context ib bp
