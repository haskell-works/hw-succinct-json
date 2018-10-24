{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.List
import Data.Semigroup                                             ((<>))
import Data.Word
import Foreign
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Json.Cursor
import HaskellWorks.Data.Json.Internal.Backend.Standard.Blank
import HaskellWorks.Data.Json.Internal.Backend.Standard.MakeIndex
import System.IO.MMap

import qualified Data.ByteString                                   as BS
import qualified Data.ByteString.Internal                          as BSI
import qualified Data.ByteString.Lazy                              as LBS
import qualified Data.Vector.Storable                              as DVS
import qualified HaskellWorks.Data.Json.Backend.Standard.SemiIndex as SSI
import qualified HaskellWorks.Data.Json.Internal.New.PreParseB     as PPB
import qualified HaskellWorks.Data.Json.Internal.New.RawIbsA       as RIBA
import qualified HaskellWorks.Data.Json.Internal.New.RawIbsB       as RIBB
import qualified HaskellWorks.Data.Json.Internal.New.Stream        as S
import qualified System.Directory                                  as IO

setupEnvJson :: FilePath -> IO BS.ByteString
setupEnvJson filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

loadJson :: BS.ByteString -> JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64))
loadJson bs = fromByteString bs :: JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64))

jsonToInterestBits3 :: [BS.ByteString] -> [BS.ByteString]
jsonToInterestBits3 = blankedJsonToInterestBits . blankJson

makeBenchBlankJson :: IO [Benchmark]
makeBenchBlankJson = do
  entries <- IO.listDirectory "corpus/bench"
  let files = ("corpus/bench/" ++) <$> (".json" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (setupEnvJson file) $ \bs -> bgroup file
      [ bench "Run blankJson" (whnf (BS.concat . blankJson) [bs])
      ]
    ]

  return (join benchmarks)

makeBenchJsonToInterestBits :: IO [Benchmark]
makeBenchJsonToInterestBits = do
  entries <- IO.listDirectory "corpus/bench"
  let files = ("corpus/bench/" ++) <$> (".json" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (setupEnvJson file) $ \bs -> bgroup file
      [ bench "Run jsonToInterestBits" (whnf (BS.concat . jsonToInterestBits3) [bs])
      ]
    ]

  return (join benchmarks)

makeBenchLoadJson :: IO [Benchmark]
makeBenchLoadJson = do
  entries <- IO.listDirectory "corpus/bench"
  let files = ("corpus/bench/" ++) <$> (".json" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (setupEnvJson file) $ \bs -> bgroup file
      [ bench "Run loadJson" (whnf loadJson bs)
      ]
    ]

  return (join benchmarks)

makeBenchBuildSemiIndexStandard :: IO [Benchmark]
makeBenchBuildSemiIndexStandard = do
  entries <- IO.listDirectory "corpus/bench"
  let files = ("corpus/bench/" ++) <$> (".json" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (setupEnvJson file) $ \bs -> bgroup file
      [ bench "Build semi index (preparse) A" (whnf SSI.semiIndexBuilder (LBS.fromStrict bs))
      ]
    ]

  return (join benchmarks)

makeBenchBuildSemiIndexPpbB :: IO [Benchmark]
makeBenchBuildSemiIndexPpbB = do
  entries <- IO.listDirectory "corpus/bench"
  let files = ("corpus/bench/" ++) <$> (".json" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (setupEnvJson file) $ \bs -> bgroup file
      [ bench "Build semi index (preparse) B" (whnf (PPB.buildSemiIndex (BS.length bs)) (S.streamByteString bs))
      ]
    ]

  return (join benchmarks)

makeBenchBuildSemiIndexRibA :: IO [Benchmark]
makeBenchBuildSemiIndexRibA = do
  entries <- IO.listDirectory "corpus/bench"
  let files = ("corpus/bench/" ++) <$> (".json" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (setupEnvJson file) $ \bs -> bgroup file
      [ bench "Build semi index (Raw IBS) A" (whnf (fst . RIBA.extractRawInterestBits) (LBS.fromStrict bs))
      ]
    ]

  return (join benchmarks)

makeBenchBuildSemiIndexRibB :: IO [Benchmark]
makeBenchBuildSemiIndexRibB = do
  entries <- IO.listDirectory "corpus/bench"
  let files = ("corpus/bench/" ++) <$> (".json" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (setupEnvJson file) $ \bs -> bgroup file
      [ bench "Build semi index (Raw IBS) B" (whnf (fst . RIBB.extractRawInterestBits) (LBS.fromStrict bs))
      ]
    ]

  return (join benchmarks)

main :: IO ()
main = do
  benchmarks <- fmap mconcat . sequence $ mempty
    -- <> pure makeBenchBlankJson
    -- <> pure makeBenchJsonToInterestBits
    -- <> pure makeBenchLoadJson
    <> pure makeBenchBuildSemiIndexStandard
    <> pure makeBenchBuildSemiIndexPpbB
    <> pure makeBenchBuildSemiIndexRibA
    <> pure makeBenchBuildSemiIndexRibB
  defaultMain benchmarks
