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

{-# LANGUAGE CApiFFI #-}

module ReadBinary (
    BinaryData,
    readBinaryFile,
    byteAt
) where

import Foreign (peekElemOff)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.C.Types
import Foreign.C.String (CString,withCString)


type BinaryData = Ptr CUChar
readBinaryFile :: String -> IO (BinaryData,Int)
byteAt :: BinaryData -> Int -> Int


foreign import capi "stdio.h fopen" fopen :: CString -> CString -> IO (Ptr CFile)
foreign import capi "stdio.h fseek" fseek :: Ptr CFile -> CLong -> CInt -> IO CInt
foreign import capi "stdio.h ftell" ftell :: Ptr CFile -> IO CLong
foreign import capi "stdio.h fread" fread :: Ptr a -> CSize -> CSize -> Ptr CFile -> IO CSize
foreign import capi "stdio.h fclose" fclose :: Ptr CFile -> IO CInt

fopen' name access =
  withCString name (\n ->
  withCString access (\a ->
  do f <- fopen n a
     if f == nullPtr
       then ioError (userError ("Unable to open the file " ++ name))
       else return f))


readBinaryFile name = do
  f <- fopen' name "rb"
  fseek f 0 2   -- seek to end of file
  len <- ftell f
  fseek f 0 0   -- seek back to beginning
  buf <- mallocBytes (fromIntegral len)
  fread buf 1 (fromIntegral len) f
  fclose f
  return (buf, fromIntegral len)


{-# INLINE byteAt #-}
buf `byteAt` ofs = fromEnum (unsafePerformIO (peekElemOff buf ofs))
