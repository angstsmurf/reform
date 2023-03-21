{-
Reform, a decompiler for Z-machine story files.
Copyright 2004 Ben Rudiak-Gould.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You can read the GNU General Public License at this URL:
     http://www.gnu.org/copyleft/gpl.html
-}


module Reform_zscii (
    getZString,
    getZChars, zCharsToUnicode,
    zsciiCharToUnicode
) where


import Reform_storyfile

import Data.Char (chr,ord)
import Data.Bits ((.&.),shiftR)
import Control.Monad (liftM,replicateM)


getZString :: StreamReader String
getZString = liftM zCharsToUnicode getZChars

getZChars :: StreamReader [Int]
getZChars = liftM zsciiUnpack getZWords

zCharsToUnicode :: [Int] -> String
zCharsToUnicode = zsciiToUnicode . zsciiDecode


getZWords = do
  eos <- isEOS
  if eos then
    return []
   else do
    w <- getUWord
    rest <- if w >= 32768 then return [] else getZWords
    return (w:rest)


zsciiUnpack :: [Int] -> [Int]

zsciiUnpack [] = []
zsciiUnpack (w:ws) =
  ((w `shiftR` 10) .&. 31) : ((w `shiftR` 5) .&. 31) : (w .&. 31) : zsciiUnpack ws


data ZsciiCode = ZsciiChar Int | ZsciiAbbrev Int


zsciiDecode :: [Int] -> [ZsciiCode]

zsciiDecode
  | ver [1,2]   = zsciiDecode12 0 0
  | ver [3..8]  = zsciiDecode3 0


zsciiDecode12 x _ (0:cs)   = ZsciiChar 32  : zsciiDecode12 x x cs

zsciiDecode12 x _ (1:cs) | hdrVersion == 1
  = ZsciiChar newline : zsciiDecode12 x x cs

zsciiDecode12 x _ (1:a:cs) | hdrVersion /= 1
  = ZsciiAbbrev a : zsciiDecode12 x x cs

-- Ambiguity in the spec: is the shift alphabet based on the current
-- shift state, or the current permanent alphabet? I assume the
-- current permanent alphabet.

zsciiDecode12 x _ (2:cs)   = zsciiDecode12 x (nextAlph x) cs
zsciiDecode12 x _ (3:cs)   = zsciiDecode12 x (prevAlph x) cs
zsciiDecode12 x _ (4:cs)   = zsciiDecode12 (nextAlph x) (nextAlph x) cs
zsciiDecode12 x _ (5:cs)   = zsciiDecode12 (prevAlph x) (prevAlph x) cs

zsciiDecode12 x 2 (6:a:b:cs) = ZsciiChar (a*32+b) : zsciiDecode12 x x cs
zsciiDecode12 x 2 (6:_)      = []

zsciiDecode12 x s (c:cs) | c >= 6 =
  (zsciiAlphabet !! s !! (c-6)) : zsciiDecode12 x x cs

zsciiDecode12 _ _ [] = []

nextAlph,prevAlph :: Int -> Int
nextAlph 2 = 0
nextAlph n = n+1
prevAlph 0 = 2
prevAlph n = n-1


zsciiDecode3 _ (0:cs)     = ZsciiChar 32 : zsciiDecode3 0 cs

zsciiDecode3 _ (1:a:cs)   = ZsciiAbbrev (00+a) : zsciiDecode3 0 cs
zsciiDecode3 _ (2:a:cs)   = ZsciiAbbrev (32+a) : zsciiDecode3 0 cs
zsciiDecode3 _ (3:a:cs)   = ZsciiAbbrev (64+a) : zsciiDecode3 0 cs

zsciiDecode3 _ (4:cs)     = zsciiDecode3 1 cs
zsciiDecode3 _ (5:cs)     = zsciiDecode3 2 cs

zsciiDecode3 2 (6:a:b:cs) = ZsciiChar (a*32+b) : zsciiDecode3 0 cs
zsciiDecode3 2 (6:_)      = []

zsciiDecode3 alph (c:cs) | c >= 6 =
  (zsciiAlphabet !! alph !! (c-6)) : zsciiDecode3 0 cs

zsciiDecode3 _ _ = []


zsciiAlphabet =
  if hdrAlphabetTableAddr /= 0 then
    map (map ZsciiChar) (evalFrom hdrAlphabetTableAddr makeAlphabetTable)
  else
    map (map (ZsciiChar . ord)) zsciiAlphabetDefault

makeAlphabetTable =
  do a <- replicateM 26 getUByte
     b <- replicateM 26 getUByte
     --(c0:c1:cs) <- replicateM 26 getUByte
     c <- replicateM 26 getUByte
     return [a,b,(head c):newline:(drop 2 c)]

newline = 13


zsciiAlphabetDefault
  | ver [1]     = ["abcdefghijklmnopqrstuvwxyz",
                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                   " 0123456789.,!?_#'\"/\\<-:()"]
  | ver [2..8]  = ["abcdefghijklmnopqrstuvwxyz",
                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                   ' ' : '\x0D' : "0123456789.,!?_#'\"/\\-:()"]


abbrevTable
  | hdrAbbrevTableAddr == 0  = []
  | otherwise =
      [evalFrom (addr*2) getZString
        | addr <- evalFrom hdrAbbrevTableAddr (repeatUntilEmpty getUWord)]


unicodeTable
  | hdrUnicodeTableAddr == 0  = defaultUnicodeTable
  | otherwise = evalFrom hdrUnicodeTableAddr getUnicodeTable

getUnicodeTable =
  do len  <- getUByte
     vals <- replicateM len getUWord
     return (map chr vals)

defaultUnicodeTable =
    "\xe4\xf6\xfc\xc4\xd6\xdc\xdf\xbb\xab\xeb\xef\xff\xcb\xcf\xe1\xe9"
 ++ "\xed\xf3\xfa\xfd\xc1\xc9\xcd\xd3\xda\xdd\xe0\xe8\xec\xf2\xf9\xc0"
 ++ "\xc8\xcc\xd2\xd9\xe2\xea\xee\xf4\xfb\xc2\xca\xce\xd4\xdb\xe5\xc5"
 ++ "\xf8\xd8\xe3\xf1\xf5\xc3\xd1\xd5\xe6\xc6\xe7\xc7\xfe\xf0\xde\xd0"
 ++ "\xa3\x153\x152\xa1\xbf"


zsciiToUnicode [] = ""

zsciiToUnicode (ZsciiChar c : cs) =
  zsciiCharToUnicode c : zsciiToUnicode cs

zsciiToUnicode (ZsciiAbbrev x : cs) =
  (abbrevTable !! x) ++ zsciiToUnicode cs


zsciiCharToUnicode :: Int -> Char

zsciiCharToUnicode c
  | c >= 155 && c <= 251  = unicodeTable !! (c - 155)
  | otherwise             = chr c
