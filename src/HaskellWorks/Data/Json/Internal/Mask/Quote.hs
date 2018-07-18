{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Data.Json.Internal.Mask.Quote
  ( makeCummulativePopCount
  , makeQuoteMaskWithCumPopCount
  , makeQuoteMask
  ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Json.Internal.Broadword
import Prelude

import qualified Data.Vector.Storable as DVS

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

makeQuoteMask :: [DVS.Vector Word64] -> [DVS.Vector Word64]
makeQuoteMask vs = makeQuoteMaskWithCumPopCount vs (makeCummulativePopCount vs)

makeQuoteMaskWithCumPopCount1 :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
makeQuoteMaskWithCumPopCount1 ibv pcv = DVS.constructN (DVS.length ibv) go
  where go :: DVS.Vector Word64 -> Word64
        go u =
          let ui = end u in
          toggle64 (pcv !!! ui) (ibv !!! ui)
{-# INLINE makeQuoteMaskWithCumPopCount1 #-}

makeQuoteMaskWithCumPopCount :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
makeQuoteMaskWithCumPopCount (ibv:ibvs) (pcv:pcvs) = makeQuoteMaskWithCumPopCount1 ibv pcv:makeQuoteMaskWithCumPopCount ibvs pcvs
makeQuoteMaskWithCumPopCount _ _                   = []
{-# INLINE makeQuoteMaskWithCumPopCount #-}

makeCummulativePopCount' :: Word64 -> DVS.Vector Word64 -> (DVS.Vector Word64, Word64)
makeCummulativePopCount' c v = let r = DVS.constructN (DVS.length v + 1) go in (DVS.unsafeInit r, DVS.unsafeLast r)
  where go :: DVS.Vector Word64 -> Word64
        go u = let ui = end u in
          if ui > 0
            then popCount1 (v !!! (ui - 1)) + (u !!! (ui - 1))
            else c
{-# INLINE makeCummulativePopCount' #-}

makeCummulativePopCount :: [DVS.Vector Word64] -> [DVS.Vector Word64]
makeCummulativePopCount = go 0
  where go :: Word64 -> [DVS.Vector Word64] -> [DVS.Vector Word64]
        go c (v:vs) = let (u, c') = makeCummulativePopCount' c v in u:go c' vs
        go _ []     = []
{-# INLINE makeCummulativePopCount #-}
