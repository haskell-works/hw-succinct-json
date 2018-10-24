module HaskellWorks.Data.Json.Internal.New.Char where

import Data.Char
import Data.Word

doubleQuote :: Word8
doubleQuote = fromIntegral (ord '"')

backSlash :: Word8
backSlash = fromIntegral (ord '\\')

openBrace :: Word8
openBrace = fromIntegral (ord '{')

closeBrace :: Word8
closeBrace = fromIntegral (ord '}')

openBracket :: Word8
openBracket = fromIntegral (ord '[')

closeBracket :: Word8
closeBracket = fromIntegral (ord ']')

comma :: Word8
comma = fromIntegral (ord ',')

colon :: Word8
colon = fromIntegral (ord ':')
