Written by Ben Rudiak-Gould. I hereby place this code in the public domain.

Usage:

MD5 hashes have the opaque type "Hash". The constant "empty" is the empty
hash (the hash of 0 bytes of data). The hash* functions (hashPtrBytes, etc.)
take a hash and a sequence of bytes (in various formats) and return a new
updated hash. The functions getAsBytes, getAsInteger, and getAsString
convert a Hash value into a usable form: a list of 16 Word8 values, a
128-bit integer, or a string of 32 hex digits, respectively.

To get the MD5 hash of the five-byte sequence 01 02 03 04 05, as a string:

    MD5.getAsString $ MD5.hashList MD5.empty ([1,2,3,4,5] :: [Word8])

The hashListIO function guarantees that its list argument will be consumed
completely before the action returns, which might be useful for lists which
are doing something unsafe behind the scenes.

Problems:

The hash values obtained from non-Word8 inputs are not portable because of
endianness and representation issues. It would be nice to fix this at least
for strings.

The hash*Range functions do not do anything particularly sensible when given
a multi-dimensional array.

Interface is somewhat cumbersome for the most common cases (but not too bad).

\begin{code}
module MD5 (
	Hash,

	empty,

	hashList,
	hashListIO,
	hashString,
	hashPtrBytes,
	hashPtrElems,
	hashStorable,
	hashStorableArray,
	hashStorableArrayRange,
	hashUArray,
	hashUArrayRange,
	hashSTUArray,
	hashSTUArrayRange,
	hashIOUArray,
	hashIOUArrayRange,

	getAsBytes,
	getAsInteger,
	getAsString
) where

import Char (intToDigit)
import Control.Monad.ST (ST,stToIO,unsafeIOToST)
import Data.Array.Base (UArray(..),STUArray(..))
import Data.Array.IArray (bounds,listArray,(!))
import Data.Array.IO.Internals (IOUArray(..))
import Data.Array.MArray (readArray)
import Data.Array.Storable (StorableArray,withStorableArray)
import Data.Bits ((.&.),shiftR)
import Data.Word (Word8,Word32)
import Foreign (Storable,sizeOf,Ptr,plusPtr,castPtr,allocaBytes,alloca,
		peekElemOff,peekArray,poke,pokeArray,unsafePerformIO)
import Foreign.C.Types (CUInt)
import Ix (Ix,index,inRange)
import GHC.Prim (ByteArray#,MutableByteArray#)

--------

newtype Hash = Hash [Word32]

contextSizeInWord32s = 4+2+16
contextSizeInBytes   = contextSizeInWord32s * 4

--------

empty :: Hash

empty = Hash [0x67452301,0xefcdab89,0x98badcfe,0x10325476,0,0]

--------

writeContext :: Ptr a -> Hash -> IO ()
writeContext ptr (Hash hash) = pokeArray (castPtr ptr) hash

readContext :: Ptr a -> IO Hash
readContext ptr =
  do bitCount <- peekElemOff (castPtr ptr :: Ptr Word32) 4
     let words = 4 + 2 + fromIntegral (((bitCount .&. 511) + 31) `shiftR` 5)
     hash <- peekArray words (castPtr ptr)
     return (Hash hash)

withContext :: Hash -> (Ptr a -> IO ()) -> IO Hash
withContext hash action =
  allocaBytes contextSizeInBytes (\ctx ->
    do writeContext ctx hash
       action ctx
       readContext ctx)

--------

hashList :: (Storable a) => Hash -> [a] -> Hash
hashList hash list =
  unsafePerformIO (hashListIO hash list)

hashListIO :: (Storable a) => Hash -> [a] -> IO Hash
hashListIO hash list =
  withContext hash (\ctx ->
    allocaBytes listChunkSize (\buf ->
      mapM_ (hashListChunk ctx buf)
            (makeListChunks listChunkElems list)))
  where listChunkSize  = 4096
        listChunkElems = listChunkSize `div` sizeOf (head list)

hashListChunk ctx buf list =
  do pokeArray buf list
     md5_update ctx buf (fromIntegral (length list))

makeListChunks size [] = [[]]
makeListChunks size xs =
  a : makeListChunks size b where (a,b) = splitAt size xs

-- FIXME: String hash should be portable, but this isn't
-- because of endianness

hashString :: Hash -> String -> Hash
hashString = hashList

hashPtrBytes :: Hash -> Ptr a -> Int -> IO Hash
hashPtrBytes hash ptr lenBytes =
  withContext hash (\ctx -> md5_update ctx ptr (fromIntegral lenBytes))

hashPtrElems :: (Storable a) => Hash -> Ptr a -> Int -> IO Hash
hashPtrElems hash ptr lenElems =
  hashPtrBytes hash ptr (lenElems * sizeOfDeref ptr)

hashStorable :: (Storable a) => Hash -> a -> Hash
hashStorable hash storable =
  unsafePerformIO $
    alloca (\ptr -> poke ptr storable >> hashPtrElems hash ptr 1)

hashStorableArray :: (Ix i, Storable e) => Hash -> StorableArray i e -> IO Hash
hashStorableArray hash array =
  withStorableArray array (\ptr ->
    hashPtrElems hash ptr (rangeSize (bounds array)))

hashStorableArrayRange :: (Ix i, Storable e) => Hash -> StorableArray i e -> (i,i) -> IO Hash
hashStorableArrayRange hash array range@(lo,hi) =
  checkRange "MD5.hashStorableArrayRange" array range $
  withStorableArray array (\ptr ->
    hashPtrElems hash (ptr `plusPtrElem` index (bounds array) lo)
                      (rangeSize range))

hashUArray :: (Ix i) => Hash -> UArray i Word8 -> Hash
hashUArray hash array = hashUArrayRange hash array (bounds array)

hashUArrayRange :: (Ix i) => Hash -> UArray i Word8 -> (i,i) -> Hash
hashUArrayRange hash array@(UArray _ _ ba) range@(lo,hi) =
  checkRange "MD5.hashUArrayRange" array range $
  unsafePerformIO $
  withContext hash (\ctx ->
    md5_update_ba ctx ba (fromIntegral (index (bounds array) lo))
                         (fromIntegral (rangeSize range)))

hashSTUArray :: (Ix i) => Hash -> STUArray s i Word8 -> ST s Hash
hashSTUArray hash array = hashSTUArrayRange hash array (bounds array)

hashSTUArrayRange :: (Ix i) => Hash -> STUArray s i Word8 -> (i,i) -> ST s Hash
hashSTUArrayRange hash array@(STUArray _ _ mba) range@(lo,hi) =
  checkRange "MD5.hashSTUArrayRange" array range $
  unsafeIOToST $
  withContext hash (\ctx ->
    md5_update_mba ctx mba (fromIntegral (index (bounds array) lo))
                           (fromIntegral (rangeSize range)))

hashIOUArray :: (Ix i) => Hash -> IOUArray i Word8 -> IO Hash
hashIOUArray hash array = hashIOUArrayRange hash array (bounds array)

hashIOUArrayRange :: (Ix i) => Hash -> IOUArray i Word8 -> (i,i) -> IO Hash
hashIOUArrayRange hash (IOUArray array) range =
  checkRange "MD5.hashIOUArrayRange" array range $
  stToIO $ hashSTUArrayRange hash array range

--------

getAsBytes :: Hash -> [Word8]
getAsBytes hash =
  unsafePerformIO $
  allocaBytes (16+contextSizeInBytes) (\ptr ->
    do writeContext (ptr `plusPtr` 16) hash
       md5_final ptr (ptr `plusPtr` 16)
       peekArray 16 (ptr :: Ptr Word8))

getAsInteger :: Hash -> Integer
getAsInteger hash =
  foldl (\hi lo -> hi * 256 + fromIntegral lo) 0 (getAsBytes hash)

getAsString :: Hash -> String
getAsString hash =
  concat [[intToDigit (fromIntegral n `shiftR` 4), intToDigit (fromIntegral n .&. 15)]
            | n <- getAsBytes hash]

--------

rangeSize :: (Ix i) => (i,i) -> Int
rangeSize range@(lo,hi) = index range hi + 1

plusPtrElem :: (Storable a) => Ptr a -> Int -> Ptr a
plusPtrElem ptr n =
  ptr `plusPtr` (n * sizeOfDeref ptr)

sizeOfDeref :: (Storable a) => Ptr a -> Int
sizeOfDeref ptr =
  sizeOf (unsafePerformIO (peekElemOff ptr undefined))

--------

checkRange id array (lo,hi) rtn =
  if inRange bds lo && inRange bds hi && index bds lo <= index bds hi + 1
    then rtn
    else error (id ++ ": index range out of bounds")
  where bds = bounds array

--------

foreign import ccall "md5_c.h MD5Final"
  md5_final :: Ptr a -> Ptr a -> IO ()
--             ^ digest ^ ctx

foreign import ccall "md5_c.h MD5Update"
  md5_update :: Ptr a -> Ptr a -> CUInt -> IO ()
--              ^ ctx    ^ buf    ^ len

foreign import ccall "md5_c.h MD5UpdateRange"
  md5_update_ba :: Ptr a -> ByteArray# -> CUInt -> CUInt -> IO ()
--                 ^ ctx    ^ buf         ^ ofs    ^ len

foreign import ccall "md5_c.h MD5UpdateRange"
  md5_update_mba :: Ptr a -> MutableByteArray# s -> CUInt -> CUInt -> IO ()

\end{code}
