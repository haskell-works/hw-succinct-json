{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MultiWayIf        #-}

module HaskellWorks.Data.Json.Internal.New.PreParseA
  ( buildSemiIndex
  , SemiIndex(..)
  ) where

import Control.Monad.ST
import Data.Word

import qualified Data.ByteString                                          as BS
import qualified Data.ByteString.Unsafe                                   as BSU
import qualified Data.Vector.Storable                                     as DVS
import qualified HaskellWorks.Data.Json.Internal.New.Bits.Writer.Storable as W
import qualified HaskellWorks.Data.Json.Internal.New.Char                 as C

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

data Context = InJson | InString | InEscape deriving (Eq, Show)

data SemiIndex v = SemiIndex
  { semiIndexContext :: !Context
  , semiIndexIb      :: !v
  , semiIndexBp      :: !v
  } deriving (Functor, Traversable, Foldable)

buildSemiIndex :: BS.ByteString -> SemiIndex (DVS.Vector Word64)
buildSemiIndex bs = DVS.createT $ do
  let len = (BS.length bs + 7) `div` 8
  mib <- W.newWriter len
  mbp <- W.newWriter (len * 2)
  buildFromByteString mib mbp bs 0 InJson
{-# INLINE buildSemiIndex #-}

buildFromByteString :: W.Writer s -> W.Writer s -> BS.ByteString -> Int -> Context -> ST s (SemiIndex (DVS.MVector s Word64))
buildFromByteString ib bp0 bs i context = if i < BS.length bs
  then do
    let c = BSU.unsafeIndex bs i
    -- let bp0 = bp
    case context of
      InJson -> if
        | c == C.openBracket || c == C.openBrace -> do
          bp1 <- W.unsafeWriteBit bp0 1
          bp2 <- W.unsafeWriteBit bp1 1
          ib' <- W.unsafeWriteBit ib 1
          buildFromByteString ib' bp2 bs (i + 1) InJson
        | c == C.closeBracket || c == C.closeBrace -> do
          bp1 <- W.unsafeWriteBit bp0 0
          bp2 <- W.unsafeWriteBit bp1 0
          ib' <- W.unsafeWriteBit ib 1
          buildFromByteString ib' bp2 bs (i + 1) InJson
        | c == C.comma || c == C.colon -> do
          bp1 <- W.unsafeWriteBit bp0 0
          bp2 <- W.unsafeWriteBit bp1 1
          ib' <- W.unsafeWriteBit ib 1
          buildFromByteString ib' bp2 bs (i + 1) InJson
        | c == C.doubleQuote -> do
          ib' <- W.unsafeWriteBit ib 0
          buildFromByteString ib' bp0 bs (i + 1) InString
        | otherwise -> do
          ib' <- W.unsafeWriteBit ib 0
          buildFromByteString ib' bp0 bs (i + 1) InJson
      InString -> do
        ib' <- W.unsafeWriteBit ib 0
        let newContext = if
              | c == C.doubleQuote  -> InJson
              | c == C.backSlash    -> InEscape
              | otherwise           -> InString
        buildFromByteString ib' bp0 bs (i + 1) newContext
      InEscape -> do
        ib' <- W.unsafeWriteBit ib 0
        buildFromByteString ib' bp0 bs (i + 1) InString
  else return (SemiIndex context (W.written ib) (W.written bp0))
{-# INLINE buildFromByteString #-}
