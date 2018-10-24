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

import qualified Data.Vector.Storable                                     as DVS
import qualified HaskellWorks.Data.Json.Internal.New.Bits.Writer.Storable as W
import qualified HaskellWorks.Data.Json.Internal.New.Char                 as C

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
                | c == C.openBracket || c == C.openBrace -> do
                  bp' <- W.unsafeWriteBits bp 2 0x3
                  ib' <- W.unsafeWriteBit ib 1
                  go step' state'' InJson ib' bp'
                | c == C.closeBracket || c == C.closeBrace -> do
                  bp' <- W.unsafeWriteBits bp 2 0x0
                  ib' <- W.unsafeWriteBit ib 1
                  go step' state'' InJson ib' bp'
                | c == C.comma || c == C.colon -> do
                  bp' <- W.unsafeWriteBits bp 2 0x2
                  ib' <- W.unsafeWriteBit ib 1
                  go step' state'' InJson ib' bp'
                | c == C.doubleQuote -> do
                  ib' <- W.unsafeWriteBit ib 0
                  go step' state'' InString ib' bp
                | otherwise -> do
                  ib' <- W.unsafeWriteBit ib 0
                  go step' state'' InJson ib' bp
              InString -> do
                ib' <- W.unsafeWriteBit ib 0
                let newContext = if
                      | c == C.doubleQuote  -> InJson
                      | c == C.backSlash    -> InEscape
                      | otherwise           -> InString
                go step' state'' newContext ib' bp
              InEscape -> do
                ib' <- W.unsafeWriteBit ib 0
                go step' state'' InString ib' bp
          Done -> return (mk2 (W.written ib) (W.written bp))
          Skip state'' -> go step' state'' context ib bp
