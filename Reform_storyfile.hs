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


module Reform_storyfile (
	evalFrom, fromTo, byteAt, wordAt, swordAt,
	storyFileLen, storyFileMD5,
	hdrVersion, hdrRelease,
	hdrInformVersion,
	hdrHighMemBase, hdrMainFuncAddr,
	hdrDictionaryAddr, hdrObjectTableAddr, hdrGlobalVarTableAddr,
	hdrStaticMemoryBaseAddr, hdrSerial, hdrAbbrevTableAddr,
	hdrAlphabetTableAddr, hdrUnicodeTableAddr,
	hdrCompiler, ZCompiler(..),
	ver, unpackRoutineAddr, unpackStringAddr, alignPaddr,
	DataBlock, StreamReader, getPos, isEOS, getTable,
	getUByte, peekUByte, getBytes, getUWord, getSWord, repeatUntilEmpty
) where


import qualified ReadBinary
import System.Environment
import qualified MD5
import qualified Data.ByteString.Lazy as L
import Data.Char (chr,ord,intToDigit,toLower)
import Control.Monad (replicateM)
import Control.Monad.State.Lazy (State,get,put,evalState)
import Data.Bits (shiftR,(.&.))
import Data.Ix (inRange)
import System.IO.Unsafe (unsafePerformIO)
import Foreign (plusPtr)

import Reform_cmdline


(storyFile,storyFileLen) =
  handleBlorb $ unsafePerformIO $
    ReadBinary.readBinaryFile storyFileName

storyFileMD5 =
  map toLower $ MD5.getAsString $ unsafePerformIO $
    MD5.hashPtrBytes MD5.empty storyFile storyFileLen
  where realLen =
          if hdrFileLength > 0 && hdrFileLength <= storyFileLen
            then hdrFileLength
            else storyFileLen


byteAt,wordAt,swordAt :: Int -> Int

byteAt = ReadBinary.byteAt storyFile

wordAt n = byteAt n * 256 + byteAt (n+1)
swordAt n =
  if w < 32768 then w else w - 65536
    where w = wordAt n


_from :: Int -> DataBlock

_from n = (n,storyFileLen)


evalFrom :: Int -> StreamReader a -> a

evalFrom n action = evalState action (_from n)


fromTo :: Int -> Int -> DataBlock
fromTo f t = (f,t)


{------------------------------- the header ----------------------------------}


hdrVersion      = byteAt 0
hdrFlags1       = byteAt 1
hdrRelease      = wordAt 2
hdrHighMemBase  = wordAt 4
hdrMainFuncAddr         = wordAt 6
hdrDictionaryAddr       = wordAt 8
hdrObjectTableAddr      = wordAt 10
hdrGlobalVarTableAddr   = wordAt 12
hdrStaticMemoryBaseAddr = wordAt 14
hdrFlags2               = wordAt 16
hdrSerial               = makeSerialString (map byteAt [18..23])
hdrAbbrevTableAddr
  | ver [1]             = 0
  | ver [2..8]          = wordAt 24
hdrFileLength           = wordAt 26 * packedAddressMultiplier
hdrFileChecksum         = wordAt 28
hdrRoutinesOffset       = 8 * wordAtVer 40 [6,7]
hdrStringsOffset        = 8 * wordAtVer 42 [6,7]
hdrTermCharTableAddr    = wordAtVer 46 [5..8]
hdrStandardVersion      = wordAt 50
hdrAlphabetTableAddr    = wordAtVer 52 [5..8]
hdrExtensionTableAddr   = wordAtVer 54 [5..8]
hdrInformVersion        = (byteAt 60 - 48) * 100 + (byteAt 62 - 48) * 10 + byteAt 63 - 48

hdrExtensionTable
  | hdrExtensionTableAddr == 0  = []
  | otherwise =
      take (head table) (tail table)
        where table = evalFrom hdrExtensionTableAddr (repeatUntilEmpty getUWord)

extHdrWord n =
  case drop n hdrExtensionTable of
    (x:_) -> x
    []    -> 0

hdrUnicodeTableAddr     = extHdrWord 3


makeSerialString bytes
  | all (inRange (0x21,0x7E)) bytes  = map chr bytes
  | otherwise  = '$' : concatMap showHex2 bytes
  where showHex2 b = [intToDigit (b `shiftR` 4), intToDigit (b .&. 15)]


{------------}


ver :: [Int] -> Bool

ver = elem hdrVersion

packedAddressMultiplier
  | ver [1..3]   = 2
  | ver [4..7]   = 4
  | ver [8]      = 8

unpackRoutineAddr, unpackStringAddr :: Int -> Int

unpackRoutineAddr p = p * packedAddressMultiplier + hdrRoutinesOffset
unpackStringAddr p = p * packedAddressMultiplier + hdrStringsOffset


wordAtVer n v = if ver v then wordAt n else 0


alignPaddr :: StreamReader ()
alignPaddr =
  do pos <- getPos
     let x = pos `mod` packedAddressMultiplier
     if x /= 0 then
       getBytes (packedAddressMultiplier-x) >> return ()
      else
       return ()


{------------}


data ZCompiler = Zilch | Inform5 | Inform6  deriving Eq

isDate x = all (>= '0') x && and (zipWith (<=) x "991939")

hdrCompiler :: ZCompiler
hdrCompiler =
  if isDate hdrSerial && head hdrSerial /= '8' then
    if byteAt 60 >= ord '6' then Inform6 else Inform5
  else
    Zilch


{-------------}


handleBlorb :: (ReadBinary.BinaryData, Int)
            -> (ReadBinary.BinaryData, Int)

handleBlorb (p,size) =
  if dwordAt 0 /= 0x464F524D then
    (p,size)
  else if dwordAt 8 /= 0x49465253 || dwordAt 12 /= 0x52496478 then
    error "Unrecognized blorb file format"
  else
    let numResources = dwordAt 16
        resources = take numResources
                     [(dwordAt n, dwordAt (n+8)) | n <- [24,36..]]
    in case [pos | (0x45786563,pos) <- resources] of
         []    -> error "No story file in blorb"
         [pos] -> case dwordAt pos of
                    0x5A434F44 -> (p `plusPtr` (pos+8), dwordAt (pos+4))
                    0x474C554C -> error "This appears to be a Glulx blorb. Try Mrifk."
                    _          -> error "Unrecognized blorb file format"
         _     -> error "More than one story file found. You'll have to extract one by hand."
  where
    byteAt  n = ReadBinary.byteAt p n
    dwordAt n = byteAt n * 16777216
              + byteAt (n+1) * 65536
              + byteAt (n+2) * 256
              + byteAt (n+3)


{-------------}


type DataBlock = (Int,Int)

type StreamReader a = State DataBlock a

getPos :: StreamReader Int
getPos =
  do (a,z) <- get
     return a

isEOS :: StreamReader Bool
isEOS =
  do (a,z) <- get
     return (a >= z)

{-# INLINE getUByte #-}
getUByte :: StreamReader Int
getUByte =
  do (a,z) <- get
     put (a+1,z)	-- should probably bounds-check
     return (byteAt a)

peekUByte :: StreamReader Int
peekUByte =
  do (a,z) <- get
     return (byteAt a)

getBytes :: Int -> StreamReader DataBlock
getBytes n =
  do (a,z) <- get
     put (a+n,z)	-- definitely bounds-check
     return (a,a+n)


{-# INLINE getUWord #-}
getUWord :: StreamReader Int
getUWord =
  do a <- getUByte
     b <- getUByte
     return (a*256+b)

getSWord :: StreamReader Int
getSWord =
  do a <- getUWord
     return (if a < 32768 then a else a - 65536)

repeatUntilEmpty :: StreamReader a -> StreamReader [a]
repeatUntilEmpty action =
  do eos <- isEOS
     if eos then
       return []
      else do
       first <- action
       s <- get
       let lazyRest = evalState (repeatUntilEmpty action) s
       return (first : lazyRest)

getTable getLength getElem =
  do len <- getLength
     replicateM len getElem
