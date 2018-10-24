module HaskellWorks.Data.Json.Internal.New.Word64 where

import Data.Word

import qualified HaskellWorks.Data.Json.Internal.New.Char as C

doubleQuote :: Word64
doubleQuote = 0x0101010101010101 * fromIntegral C.doubleQuote

backSlack :: Word64
backSlack = 0x0101010101010101 * fromIntegral C.backSlash

openBrace :: Word64
openBrace = 0x0101010101010101 * fromIntegral C.openBrace

closeBrace :: Word64
closeBrace = 0x0101010101010101 * fromIntegral C.closeBrace

openBracket :: Word64
openBracket = 0x0101010101010101 * fromIntegral C.openBracket

closeBracket :: Word64
closeBracket = 0x0101010101010101 * fromIntegral C.closeBracket

comma :: Word64
comma = 0x0101010101010101 * fromIntegral C.comma

colon :: Word64
colon = 0x0101010101010101 * fromIntegral C.colon
