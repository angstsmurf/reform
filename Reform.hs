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


module Main () where


import Reform_cmdline
import Reform_symfile
import Reform_storyfile
import Reform_disasm
import Reform_decompile
import Reform_objects
import Reform_highmem
import Reform_grammar
import Reform_print
import Reform_util
import Reform_zcode
import Reform_zscii

import Data.Char (toLower)
import Control.Monad (unless)
import Data.Array ((!),elems)
import Data.Tree
import Data.Maybe
import Numeric (showHex)
import System (exitFailure)
import IO


{-------------------------------------------------------------------------}


reformRelease = "6"
reformSerial  = "040226"


main =
  do checkVersion
     checkMD5
     if generateOutputSymFile
       then writeSymFile
       else writeInformSource


writeInformSource =
  do putStrLn ("! \""++storyFileName++"\", "++show storyFileLen++" bytes")
     putStrLn ("! Z-machine version "++show hdrVersion++", release "++show hdrRelease++", serial "++hdrSerial)
     putStrLn "!"
     putStrLn ("! Decompiled by Reform release "++reformRelease++", serial "++reformSerial)
     case syms of
       (_:_) -> putStrLn ("! using symbol file \""++usedSymFileName++"\"")
       []    -> return ()
--   putStrLn "\n! Dictionary\n"
--   mapM_ (putStrLn . ppDictEntry) (elems dictionary)
     putStrLn "\n! Grammar"
     mapM_ putStrLn (ppVerbs verbs)
     putStrLn "\n! Object tree\n"
     mapM_ putStrLn (concatMap ppObjectTree objectForest)
     putStrLn "\n! Constants"
     mapM_ putStrLn (concatMap ppConstants [x | x@(SFEnum _ _) <- syms])
     putStrLn "\n! Globals\n"
     mapM_ putStrLn (concatMap ppGlobal [0..lastGlobal])
     putStrLn "\n! Arrays"
     mapM_ putStrLn (concatMap ppArray [x | x@(SFArray _ _ _) <- syms])
     mapM_ putStrLn (concatMap ppHimem himemContents)


writeSymFile = mapM_ putStrLn outputSymFile


checkVersion =
  unless (ver [1..8]) $
    do hPutStr stderr ('"' : storyFileName)
       hPutStr stderr "\" does not appear to be a Z-machine story file.\n"
       exitFailure


checkMD5 =
  case [md5 | SFMD5 md5 <- syms] of
    []   -> return ()
    md5s ->
      unless (storyFileMD5 `elem` (map (map toLower) md5s)) $
        do hPutStr stderr "\
            \Error: The MD5 checksum of the story file does not match the value\n\
            \stored in the symbol file. If you're sure that the story file and\n\
            \symbol file are correctly matched, add this line to the symbol file\n\
            \and run Reform again:\n\n\tMD5  "
           hPutStrLn stderr storyFileMD5
           exitFailure


ppObjectTree = ppObjectTree' 0

ppObjectTree' depth (Node obj children) =
  ppObject depth obj (objects ! obj) ++ concatMap (ppObjectTree' (depth+1)) children


ppHimem (HimemRoutines routines) =
  concat [ppRoutine addr params (decompile body)
    | (addr,(params,body)) <- routines]

ppHimem (HimemStrings strings) =
  "" : map ppHighString strings

ppHimem (HimemComment msgs) = "" : msgs


ppHighString (ad,s) =
  "! " ++ showDecHex ad ++ ": \"" ++ ppString s ++ "\""


ppConstants (SFEnum name (EnumInfo vals zero)) =
  "" : ("! " ++ name) : mapMaybe helper ((0,zero) : reverse vals)
  where
    helper (num,name)
      | name == show num  = Nothing
      | otherwise         = Just ("Constant " ++ name ++ " = " ++ shows num ";")
