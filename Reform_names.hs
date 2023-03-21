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


module Reform_names (
	nameObject, nameProp, nameAttr,
	nameRoutine, nameLocal, nameGlobal,
	nameAction, nameArray, namePrintFunc,
	sfRoutineNamed, sfGlobalNamed, sfObjectNamed,
	sfAttrNamed, sfPropNamed, sfActionNamed
) where


import Reform_storyfile
import Reform_symfile
import Reform_zscii
import Reform_util
import Reform_objects
import Reform_grammar

import Data.Char (toLower)
import Control.Monad (liftM,liftM4,replicateM)
import Control.Monad.State (evalState)
import Data.Array
import Data.Ix (inRange)
import Data.Maybe (fromMaybe,fromJust,isJust)


{--------------------}


nameObject :: Int -> String
nameObject n
  | n == 0         = "nothing"
  | n > numObjects = "invalidObj" ++ show n
  | otherwise = objectNames ! n

objectNames =
  accumArray (\a b -> b) undefined (1,numObjects) $
    -- later in the list takes precedence
    map guessObjName (assocs objects) ++
    inform6ClassNames ++
    sfObjectNames

guessObjName (n,obj) =
  case obj of
    ("",_,_)   -> (n,baseName)
    (desc,_,_) -> (n,baseName ++ '_' : makeIdent desc)
  where baseName = "obj" ++ show n

makeIdent name =
  map helper name
  where helper c = if isLegalIdentChar c then c else '_'

isLegalIdentChar c =
  inRange ('0','9') c || inRange ('A','Z') c || inRange ('a','z') c


sfObjectNames = [(n,t) | SFObjName n t <- syms]


sfObjectNamed :: Int -> Bool
sfObjectNamed n = isJust (n `tableLookup` sfObjectNamesTable)
sfObjectNamesTable = makeLookupTable sfObjectNames


{--------------------}


nameProp :: Int -> String
nameProp prop =
  fromMaybe ("prop" ++ show prop)
            (prop `tableLookup` propNames)

propNames =
  makeLookupTable $
    -- earlier in the list takes precedence
    sfPropNames ++
    filter (not.null.snd) (zip [1..] inform6PropNames) ++
    dirPropNames

sfPropNames = [(n,t) | SFProperty n t _ <- syms]

dirPropNames =
  [(prop,word++"_to") | (_,word,tags) <- elems dictionary,
                        DictDir prop  <- tags]


sfPropNamed :: Int -> Bool
sfPropNamed n = isJust (n `tableLookup` sfPropNamesTable)
sfPropNamesTable = makeLookupTable sfPropNames


{--------------------}


nameAttr :: Int -> String
nameAttr attr
  | attr >= 0 && attr <= 47  = attrNames ! attr
  | otherwise                = "invalidAttrib" ++ show attr

attrNames =
  accumArray (\a b -> b) undefined (0,47) $
    -- later in the list takes precedence
    [(n, "attrib" ++ show n) | n <- [0..47]] ++
    zip [0..] inform6AttrNames ++
    sfAttrNames


sfAttrNames = [(n,t) | SFAttrName n t <- syms]


sfAttrNamed :: Int -> Bool
sfAttrNamed n = isJust (n `tableLookup` sfAttrNamesTable)
sfAttrNamesTable = makeLookupTable sfAttrNames


{--------------------}


nameRoutine :: Int -> String
nameRoutine addr =
  fromMaybe ("routine" ++ show addr)
            (addr `tableLookup` routineNames)

routineNames =
  makeLookupTable $
    -- earlier in the list takes precedence
    sfRoutineNames ++
    [(unpackRoutineAddr addr, nameAction n ++ "Sub")
       | addr <- actionRoutines | n <- [0..]] ++
    [(unpackRoutineAddr addr, "Pre" ++ nameAction n ++ "Sub")
       | addr <- preactionRoutines | n <- [0..]] ++
    dirPropNames


sfRoutineNames = [(a,n) | SFRoutine a n _ _ _ <- syms]


sfRoutineNamed :: Int -> Bool
sfRoutineNamed n = isJust (n `tableLookup` sfRoutineNamesTable)
sfRoutineNamesTable = makeLookupTable sfRoutineNames


{--------------------}


nameLocal :: Int -> Int -> String
nameLocal routine n =
  fromMaybe ("local" ++ show n)
            (sfLocalName routine n)


sfLocalName :: Int -> Int -> Maybe String
sfLocalName f n =
  case f `tableLookup` routineArgNames of
    Nothing    -> Nothing
    Just names -> case drop (n-1) names of
                    []    -> Nothing
                    (n:_) -> Just n

routineArgNames =
  makeLookupTable [(a,ts) | SFRoutine a _ _ ts _ <- syms]


{--------------------}


nameGlobal :: Int -> String
nameGlobal n
  | inRange (bounds globalNames) n
      = fromMaybe ("global" ++ show n) (globalNames ! n)
  | otherwise
      = "invalidGlobal" ++ show n

sfGlobalNamed :: Int -> Bool
sfGlobalNamed n = isJust (globalNames ! n)

globalNames =
  accumArray (\a b -> b) Nothing (0,lastGlobal)
    [(n,Just t) | SFGlobal n t _ <- syms]


{--------------------}


nameAction :: Int -> String
nameAction n =
  fromMaybe ("Action" ++ show n) (n `tableLookup` actionNames)

actionNames =
  makeLookupTable $
    -- earlier in the list takes precedence
    sfActionNames ++ zip [0..] inform6ActionNames


sfActionNames = [(n,t) | SFActionName n t <- syms]


sfActionNamed :: Int -> Bool
sfActionNamed n = isJust (n `tableLookup` sfActionNamesTable)
sfActionNamesTable = makeLookupTable sfActionNames


{--------------------}


nameArray :: Int -> Maybe String

nameArray n
  | n < 0x40  = Just (show n)
  | otherwise = n `tableLookup` arrayNames


arrayNames =
  makeLookupTable sfArrayNames

sfArrayNames = [(n,t) | SFArray n t _ <- syms]


{--------------------}


nameActionSub addr = Nothing	-- FIXME


namePrintFunc :: Int -> String
namePrintFunc f =
  fromJust (sfPrintFunc (unpackRoutineAddr f))

{-
  case map toLower (nameRoutine f) `lookup` specialPrintFuncNames of
    Just name -> name
    Nothing   -> nameRoutine f

specialPrintFuncNames =
 [("rt__chprintc",	"char"),
  ("rt__chprinta",	"address"),
  ("rt__chprints",	"string"),
  ("rt__chprinto",	"object"),
  ("defart",		"the"),
  ("indefart",		"a"),
  ("cdefart",		"The"),
  ("printshortname",	"name"),
  ("englishnumber",	"number"),
  ("print__pname",	"property")]
-}


{--------------------}


inform6ClassNames =
  [(num,name) | num <- inform6ClassNumbers, (name,_,_) <- [objects!num]]


(inform6ClassNumbers,inform6PropNames,inform6AttrNames,inform6ActionNames) =
  case compiler of
    Inform6 -> evalFrom afterObjectTable
                        (liftM4 (,,,) getI6ClassNumbers getI6PropNames
                                      getI6AttrNames getI6ActionNames)
    _       -> ([],[],[],[])


getI6ClassNumbers =
  do n <- getUWord
     if n == 0 then return []
               else liftM (n:) getI6ClassNumbers

getI6PropNames =
  do count <- getUWord
     replicateM (count-1) getZStringIndirect

getI6AttrNames | hdrInformVersion < 610  = return []
               | otherwise  = replicateM 48 getZStringIndirect

getI6ActionNames | hdrInformVersion < 610  = return []	-- FIXME: ???
                 | otherwise  = replicateM (maxAction+1) getZStringIndirect	-- FIXME: fake actions
  where maxAction =
          maximum [act | (_,lines) <- verbs, GrammarLine _ act _ <- lines]


getZStringIndirect =
  do n <- getUWord
     if n == 0 then return ""
               else return (evalFrom (unpackStringAddr n) getZString)
