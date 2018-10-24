{-# LANGUAGE BinaryLiterals #-}

module HaskellWorks.Data.Json.Internal.New.Internal where

import Data.Word

import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

escapeMask :: DV.Vector (DVS.Vector Word8)
escapeMask = DV.fromList
  [ DVS.fromList
    [ 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11110111, 0b11110101, 0b11111111, 0b11110111
    , 0b11101111, 0b11101101, 0b11101011, 0b11101111, 0b11111111, 0b11111101, 0b11101111, 0b11111111
    , 0b11011111, 0b11011101, 0b11011011, 0b11011111, 0b11010111, 0b11010101, 0b11011111, 0b11010111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11011111, 0b11011101, 0b11111111, 0b11011111
    , 0b10111111, 0b10111101, 0b10111011, 0b10111111, 0b10110111, 0b10110101, 0b10111111, 0b10110111
    , 0b10101111, 0b10101101, 0b10101011, 0b10101111, 0b10111111, 0b10111101, 0b10101111, 0b10111111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11110111, 0b11110101, 0b11111111, 0b11110111
    , 0b10111111, 0b10111101, 0b10111011, 0b10111111, 0b11111111, 0b11111101, 0b10111111, 0b11111111
    , 0b01111111, 0b01111101, 0b01111011, 0b01111111, 0b01110111, 0b01110101, 0b01111111, 0b01110111
    , 0b01101111, 0b01101101, 0b01101011, 0b01101111, 0b01111111, 0b01111101, 0b01101111, 0b01111111
    , 0b01011111, 0b01011101, 0b01011011, 0b01011111, 0b01010111, 0b01010101, 0b01011111, 0b01010111
    , 0b01111111, 0b01111101, 0b01111011, 0b01111111, 0b01011111, 0b01011101, 0b01111111, 0b01011111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11110111, 0b11110101, 0b11111111, 0b11110111
    , 0b11101111, 0b11101101, 0b11101011, 0b11101111, 0b11111111, 0b11111101, 0b11101111, 0b11111111
    , 0b01111111, 0b01111101, 0b01111011, 0b01111111, 0b01110111, 0b01110101, 0b01111111, 0b01110111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b01111111, 0b01111101, 0b11111111, 0b01111111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11110111, 0b11110101, 0b11111111, 0b11110111
    , 0b11101111, 0b11101101, 0b11101011, 0b11101111, 0b11111111, 0b11111101, 0b11101111, 0b11111111
    , 0b11011111, 0b11011101, 0b11011011, 0b11011111, 0b11010111, 0b11010101, 0b11011111, 0b11010111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11011111, 0b11011101, 0b11111111, 0b11011111
    , 0b10111111, 0b10111101, 0b10111011, 0b10111111, 0b10110111, 0b10110101, 0b10111111, 0b10110111
    , 0b10101111, 0b10101101, 0b10101011, 0b10101111, 0b10111111, 0b10111101, 0b10101111, 0b10111111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11110111, 0b11110101, 0b11111111, 0b11110111
    , 0b10111111, 0b10111101, 0b10111011, 0b10111111, 0b11111111, 0b11111101, 0b10111111, 0b11111111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11110111, 0b11110101, 0b11111111, 0b11110111
    , 0b11101111, 0b11101101, 0b11101011, 0b11101111, 0b11111111, 0b11111101, 0b11101111, 0b11111111
    , 0b11011111, 0b11011101, 0b11011011, 0b11011111, 0b11010111, 0b11010101, 0b11011111, 0b11010111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11011111, 0b11011101, 0b11111111, 0b11011111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11110111, 0b11110101, 0b11111111, 0b11110111
    , 0b11101111, 0b11101101, 0b11101011, 0b11101111, 0b11111111, 0b11111101, 0b11101111, 0b11111111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11110111, 0b11110101, 0b11111111, 0b11110111
    , 0b11111111, 0b11111101, 0b11111011, 0b11111111, 0b11111111, 0b11111101, 0b11111111, 0b11111111
    ]
  , DVS.fromList
    [ 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11110110, 0b11110111, 0b11111110, 0b11111111
    , 0b11101110, 0b11101111, 0b11101010, 0b11101011, 0b11111110, 0b11111111, 0b11101110, 0b11101111
    , 0b11011110, 0b11011111, 0b11011010, 0b11011011, 0b11010110, 0b11010111, 0b11011110, 0b11011111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11011110, 0b11011111, 0b11111110, 0b11111111
    , 0b10111110, 0b10111111, 0b10111010, 0b10111011, 0b10110110, 0b10110111, 0b10111110, 0b10111111
    , 0b10101110, 0b10101111, 0b10101010, 0b10101011, 0b10111110, 0b10111111, 0b10101110, 0b10101111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11110110, 0b11110111, 0b11111110, 0b11111111
    , 0b10111110, 0b10111111, 0b10111010, 0b10111011, 0b11111110, 0b11111111, 0b10111110, 0b10111111
    , 0b01111110, 0b01111111, 0b01111010, 0b01111011, 0b01110110, 0b01110111, 0b01111110, 0b01111111
    , 0b01101110, 0b01101111, 0b01101010, 0b01101011, 0b01111110, 0b01111111, 0b01101110, 0b01101111
    , 0b01011110, 0b01011111, 0b01011010, 0b01011011, 0b01010110, 0b01010111, 0b01011110, 0b01011111
    , 0b01111110, 0b01111111, 0b01111010, 0b01111011, 0b01011110, 0b01011111, 0b01111110, 0b01111111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11110110, 0b11110111, 0b11111110, 0b11111111
    , 0b11101110, 0b11101111, 0b11101010, 0b11101011, 0b11111110, 0b11111111, 0b11101110, 0b11101111
    , 0b01111110, 0b01111111, 0b01111010, 0b01111011, 0b01110110, 0b01110111, 0b01111110, 0b01111111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b01111110, 0b01111111, 0b11111110, 0b11111111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11110110, 0b11110111, 0b11111110, 0b11111111
    , 0b11101110, 0b11101111, 0b11101010, 0b11101011, 0b11111110, 0b11111111, 0b11101110, 0b11101111
    , 0b11011110, 0b11011111, 0b11011010, 0b11011011, 0b11010110, 0b11010111, 0b11011110, 0b11011111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11011110, 0b11011111, 0b11111110, 0b11111111
    , 0b10111110, 0b10111111, 0b10111010, 0b10111011, 0b10110110, 0b10110111, 0b10111110, 0b10111111
    , 0b10101110, 0b10101111, 0b10101010, 0b10101011, 0b10111110, 0b10111111, 0b10101110, 0b10101111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11110110, 0b11110111, 0b11111110, 0b11111111
    , 0b10111110, 0b10111111, 0b10111010, 0b10111011, 0b11111110, 0b11111111, 0b10111110, 0b10111111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11110110, 0b11110111, 0b11111110, 0b11111111
    , 0b11101110, 0b11101111, 0b11101010, 0b11101011, 0b11111110, 0b11111111, 0b11101110, 0b11101111
    , 0b11011110, 0b11011111, 0b11011010, 0b11011011, 0b11010110, 0b11010111, 0b11011110, 0b11011111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11011110, 0b11011111, 0b11111110, 0b11111111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11110110, 0b11110111, 0b11111110, 0b11111111
    , 0b11101110, 0b11101111, 0b11101010, 0b11101011, 0b11111110, 0b11111111, 0b11101110, 0b11101111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11110110, 0b11110111, 0b11111110, 0b11111111
    , 0b11111110, 0b11111111, 0b11111010, 0b11111011, 0b11111110, 0b11111111, 0b11111110, 0b11111111
    ]
  ]
{-# NOINLINE escapeMask #-}
