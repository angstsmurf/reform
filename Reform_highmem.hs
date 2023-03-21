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


module Reform_highmem (
    himemContents, HimemType(..),
    isKnownRoutinePaddr, isKnownStringPaddr,
    routines
) where


import Reform_util
import Reform_zcode
import Reform_storyfile
import Reform_symfile
import Reform_disasm
import Reform_zscii

import Control.Monad (liftM2)
import Control.Monad.State (evalState)
import Data.Maybe (isJust)
import Numeric (showHex)


--                               addr params body
data HimemType = HimemRoutines [(Int,([Int],[ZInstr]))]
               | HimemStrings  [(Int,String)]
               | HimemComment  [String]


himemContents :: [HimemType]

himemContents =
  case concatMap decodeArea syms of
    [] -> evalFrom hdrHighMemBase getRoutinesAndStrings
    xs -> xs
  where
    decodeArea (SFCodeArea block) =
      HimemComment ["! CodeArea " ++ showBlock block]
       : evalState disasmRoutineBlock block
    decodeArea (SFStringArea block) =
      [HimemComment ["! StringArea " ++ showBlock block],
       HimemStrings (evalState getStrings block)]
    decodeArea _ = []


routines :: [(Int,([Int],[ZInstr]))]
routines = concat [rs | HimemRoutines rs <- himemContents]


showBlock (from,to) =
  "0x" ++ showHex from (" 0x" ++ showHex to "")


getRoutinesAndStrings =
  do routines <- disasmRoutines
     strings  <- getStrings
     let lastRoutine | null routines = Nothing
                     | otherwise     = Just (fst (last routines))
     return [codeStart,
             HimemRoutines routines,
             warning lastRoutine,
             HimemStrings strings]
  where codeStart = HimemComment ["! Routines"]
        warning Nothing = HimemComment
          ["! Code appears to end here. No routines found."]
        warning (Just addr) = HimemComment
          ["! Code appears to end here. If the strings below look like",
           "! garbage, try adding this directive to the symbol file:",
           "!   FalseEnd\t0x" ++ showHex addr ('\t' : show (sfFalseEnd addr + 1))]


getStrings =
  repeatUntilEmpty (alignPaddr >> liftM2 (,) getPos getZString)


findNonZero =
  do x <- isEOS
     if x
     then return False
     else do
       x <- peekUByte
       if x /= 0
       then return True
       else findNonZero


isKnownRoutinePaddr paddr =
  isJust (unpackRoutineAddr paddr `tableLookup` routineLookup)

isKnownStringPaddr paddr =
  isJust (unpackStringAddr paddr `tableLookup` stringLookup)

routineLookup =
  makeLookupTable $ concat [rs | HimemRoutines rs <- himemContents]
stringLookup  =
  makeLookupTable $ concat [ss | HimemStrings ss <- himemContents]


disasmRoutines =
  do alignPaddr
     x <- routineMayFollow
     if not x
     then return []
     else do
       r <- disasmRoutine
       rs <- disasmRoutines
       return (r : rs)


disasmRoutineBlock =
  do routines <- disasmRoutines
     x <- isEOS
     let lastRoutine | null routines = Nothing
                     | otherwise     = Just (fst (last routines))
     if x then return [HimemRoutines routines]
          else return [HimemRoutines routines, incomplete lastRoutine]
  where incomplete Nothing = HimemComment
          ["! Could not disassemble the CodeArea."]
        incomplete (Just addr) = HimemComment
          ["! Could not disassemble the remainder of the CodeArea.",
           "! Try adding this directive to the symbol file:",
           "!   FalseEnd\t0x" ++ showHex addr ('\t' : show (sfFalseEnd addr + 1))]
