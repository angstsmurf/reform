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


module Reform_cmdline (
	storyFileName,
	symFileName,
	generateOutputSymFile,
	showConstAddrs
) where


import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)


storyFileName :: String
symFileName   :: Maybe String
generateOutputSymFile :: Bool
showConstAddrs        :: Bool

(storyFileName,symFileName,generateOutputSymFile,showConstAddrs) =
  unsafePerformIO parseArgs


parseArgs =
  do args <- getArgs
     args <- parseArgs' (Nothing,Nothing,False,False) args
     case args of
       (Nothing,_,_,_) -> usage
       (Just story,sym,outsym,sa) -> return (story,sym,outsym,sa)

parseArgs' result [] = return result

parseArgs' (x,Nothing,y,z) ("-s" : symfile : rest) =
  parseArgs' (x,Just symfile,y,z) rest

parseArgs' (x,y,False,z) ("-t" : rest) =
  parseArgs' (x,y,True,z) rest

parseArgs' (x,y,z,False) ("-x" : rest) =
  parseArgs' (x,y,z,True) rest

parseArgs' _ (('-' : _) : _) = usage

parseArgs' (Nothing,x,y,z) (story : rest) =
  parseArgs' (Just story,x,y,z) rest

parseArgs' _ _ = usage


usage =
  do putStrLn "usage: reform storyfile.z5 [-s symfile.reform] [-t] [-x]"
     exitFailure
