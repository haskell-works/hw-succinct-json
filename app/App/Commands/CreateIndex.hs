{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Commands.Types
import Control.Lens
import Data.Maybe
import Data.Semigroup      ((<>))
import Data.Word
import Foreign
import Options.Applicative hiding (columns)

import qualified App.Lens                                           as L
import qualified Data.ByteString                                    as BS
import qualified Data.ByteString.Builder                            as B
import qualified Data.ByteString.Internal                           as BSI
import qualified Data.ByteString.Lazy                               as LBS
import qualified Data.Vector.Fixed                                  as DVF
import qualified Data.Vector.Storable                               as DVS
import qualified HaskellWorks.Data.ByteString                       as BS
import qualified HaskellWorks.Data.ByteString.Lazy                  as LBS
import qualified HaskellWorks.Data.Json.Internal.Blank              as J
import qualified HaskellWorks.Data.Json.Internal.BlankedJson        as J
import qualified HaskellWorks.Data.Json.Internal.MakeIndex          as J
import qualified HaskellWorks.Data.Json.Internal.New.PreParseA      as PPA
import qualified HaskellWorks.Data.Json.Internal.New.PreParseB      as PPB
import qualified HaskellWorks.Data.Json.Internal.New.RawIbsA        as RIBA
import qualified HaskellWorks.Data.Json.Internal.New.RawIbsB        as RIBB
import qualified HaskellWorks.Data.Json.Internal.New.RawIbsC        as RIBC
import qualified HaskellWorks.Data.Json.Internal.New.RawIbsD        as RIBD
import qualified HaskellWorks.Data.Json.Internal.New.RawIbsE        as RIBE
import qualified HaskellWorks.Data.Json.Internal.New.Stream         as S
import qualified HaskellWorks.Data.Json.Internal.ToBalancedParens64 as J
import qualified System.IO                                          as IO
import qualified System.IO.MMap                                     as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

runCreateIndexOriginal :: CreateIndexOptions -> IO ()
runCreateIndexOriginal opts = do
  let filePath = opts ^. L.filePath
  let outputIbFile = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let blankedJson = J.blankJson [bs]
  let ibs = LBS.fromChunks (J.blankedJsonToInterestBits blankedJson)
  let bps = J.toBalancedParens64 (J.BlankedJson blankedJson)
  let vb = DVS.foldl (\b a -> b <> B.word64LE a) mempty bps
  LBS.writeFile outputIbFile ibs
  h <- IO.openFile outputBpFile IO.WriteMode
  B.hPutBuilder h vb
  IO.hClose h

runCreateIndexPreparseA :: CreateIndexOptions -> IO ()
runCreateIndexPreparseA opts = do
  let filePath = opts ^. L.filePath
  let outputIbFile = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let PPA.SemiIndex _ ibs bps = PPA.buildSemiIndex bs
  LBS.writeFile outputIbFile (LBS.toLazyByteString ibs)
  LBS.writeFile outputBpFile (LBS.toLazyByteString bps)

runCreateIndexPreparseB :: CreateIndexOptions -> IO ()
runCreateIndexPreparseB opts = do
  let filePath = opts ^. L.filePath
  let outputIbFile = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let si = PPB.buildSemiIndex size (S.streamByteString bs)
  let ibs = si DVF.! 0
  let bps = si DVF.! 1
  LBS.writeFile outputIbFile (LBS.toLazyByteString ibs)
  LBS.writeFile outputBpFile (LBS.toLazyByteString bps)

runCreateIndexRibA :: CreateIndexOptions -> IO ()
runCreateIndexRibA opts = do
  let filePath = opts ^. L.filePath
  let outputIbFile = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let (ibs, bps) = RIBA.extractRawInterestBits (LBS.fromStrict bs)
  LBS.writeFile outputIbFile (LBS.toLazyByteString (S.unstreamVector ((BS.length bs + 63) `div` 64) ibs))
  LBS.writeFile outputBpFile (LBS.toLazyByteString (S.unstreamVector ((BS.length bs + 63) `div` 64) bps))

runCreateIndexRibB :: CreateIndexOptions -> IO ()
runCreateIndexRibB opts = do
  let filePath = opts ^. L.filePath
  let outputIbFile = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let (ibs, bps) = RIBB.extractRawInterestBits (LBS.fromStrict bs)
  LBS.writeFile outputIbFile (LBS.toLazyByteString (S.unstreamVector ((BS.length bs + 63) `div` 64) ibs))
  LBS.writeFile outputBpFile (LBS.toLazyByteString (S.unstreamVector ((BS.length bs + 63) `div` 64) bps))

runCreateIndexRibC :: CreateIndexOptions -> IO ()
runCreateIndexRibC opts = do
  let filePath = opts ^. L.filePath
  let outputIbFile = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let (ibs, bps) = RIBC.extractRawInterestBits (LBS.fromStrict bs)
  IO.withFile outputIbFile IO.WriteMode $ flip B.hPutBuilder (foldMap (B.byteString . BS.toByteString) ibs)
  LBS.writeFile outputBpFile (LBS.toLazyByteString (S.unstreamVector ((BS.length bs + 63) `div` 64) bps))

runCreateIndexRibD :: CreateIndexOptions -> IO ()
runCreateIndexRibD opts = do
  let filePath = opts ^. L.filePath
  let outputIbFile = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let ibs = RIBD.extractRawInterestBits (LBS.fromStrict bs)
  IO.withFile outputIbFile IO.WriteMode $ flip B.hPutBuilder (foldMap (B.byteString . BS.toByteString) ibs)

runCreateIndexRibE :: CreateIndexOptions -> IO ()
runCreateIndexRibE opts = do
  let filePath = opts ^. L.filePath
  let outputIbFile = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let ibs = RIBE.extractRawInterestBits (LBS.fromStrict bs)
  IO.withFile outputIbFile IO.WriteMode $ flip B.hPutBuilder (foldMap (B.byteString . BS.toByteString) ibs)

runCreateIndex :: CreateIndexOptions -> IO ()
runCreateIndex opts = case opts ^. L.method of
  "original"   -> runCreateIndexOriginal  opts
  "preparse-a" -> runCreateIndexPreparseA opts
  "preparse-b" -> runCreateIndexPreparseB opts
  "rib-a"      -> runCreateIndexRibA      opts
  "rib-b"      -> runCreateIndexRibB      opts
  "rib-c"      -> runCreateIndexRibC      opts
  "rib-d"      -> runCreateIndexRibD      opts
  "rib-e"      -> runCreateIndexRibE      opts
  unknown      -> IO.hPutStrLn IO.stderr $ "Unknown method " <> show unknown

optsCreateIndex :: Parser CreateIndexOptions
optsCreateIndex = CreateIndexOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input JSON file"
        <>  metavar "STRING"
        )
  <*> strOption
        (   long "method"
        <>  short 'm'
        <>  value "original"
        <>  help "Method for creating index"
        <>  metavar "STRING"
        )
  <*> optional
        ( strOption
          (   long "output-ib-file"
          <>  help "Filename for output ib index"
          <>  metavar "STRING"
          )
        )
  <*> optional
        ( strOption
          (   long "output-bp-file"
          <>  help "Filename for output bp index"
          <>  metavar "STRING"
          )
        )

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
