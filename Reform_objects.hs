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


module Reform_objects (
	Object, PropVal(..),
	objects, numObjects, objectForest,
	propertyDefaults,
	propType,
	afterObjectTable
) where


import Reform_storyfile
import Reform_symfile
import Reform_zscii
import Reform_zcode
import Reform_grammar
import Reform_highmem
import Reform_util

import Data.Array (Array,listArray,(!),elems)
import Control.Monad (replicateM,liftM)
import Control.Monad.State (evalState,runState)
import Data.Bits ((.&.),shiftR,testBit)
import Data.Tree (Tree(..),Forest)
import Debug.Trace (trace)
import Data.Ix (inRange)
import List (sortBy,partition)
import Data.Maybe (fromMaybe,maybeToList)
import Numeric (showHex)


-- short name, attribs, properties
type Object = (String,Integer,[(Int,PropVal)])

data PropVal
  = PropValBlock Int DataBlock String	-- string is comment
  | PropValList [(ZType,Int)]
  | PropValCEXIT Int Int Int	-- obj, global, string paddr (see "Learning ZIL")
  | PropValDEXIT Int Int Int	-- obj, doorobj, string paddr (see "Learning ZIL")

-- obj, parent, sibling, child
type Object' = (Object,Int,Int,Int)


objectForest :: Forest Int

objectForest =
  map treeFrom roots
  where parents  = listArray (1,numObjects) [p | (_,p,_,_) <- objectList]
        siblings = listArray (1,numObjects) [s | (_,_,s,_) <- objectList]
        children = listArray (1,numObjects) [c | (_,_,_,c) <- objectList]
        roots    = [n | n <- [1..numObjects], parents!n == 0]
        treeFrom n =
          let kids = takeWhile (/= 0) $ iterate (siblings!) (children!n)
          in Node n (map treeFrom kids)


propertyDefaults :: [Int]
afterObjectTable :: Int
(propertyDefaults, rawObjectList, afterObjectTable) =
  evalFrom hdrObjectTableAddr objectTableDecode

objects :: Array Int Object
objects = listArray (1,numObjects) [obj | (obj,_,_,_) <- objectList]

numObjects :: Int
numObjects = length objectList


{-----------}


oldObjectFormat = ver [1..3]


objectTableDecode :: StreamReader ([Int],[Object'],Int)

objectTableDecode = do
  propertyDefaults <- replicateM (if oldObjectFormat then 31 else 63) getUWord
  objects <- objectTableDecode' [] 0x100000
  pos <- getPos
  let (objects',propEnds) = unzip (map objectTableDecode'' (guessClasses objects))
  return (propertyDefaults, objects', maximum (pos:propEnds))

objectTableDecode' sofar minPropAddr = do
  pos      <- getPos
  if pos >= minPropAddr then return (reverse sofar) else do
  attrs    <- getAttrs
  parent   <- if oldObjectFormat then getUByte else getUWord
  sibling  <- if oldObjectFormat then getUByte else getUWord
  child    <- if oldObjectFormat then getUByte else getUWord
  propAddr <- getUWord
  objectTableDecode' ((attrs,propAddr,parent,sibling,child) : sofar)
                     (propAddr `min` minPropAddr)

getAttrs =
  do a1 <- getUWord
     a2 <- getUWord
     a3 <- if oldObjectFormat then return 0 else getUWord
     return (fromIntegral a1 * 0x100000000 + fromIntegral a2 * 0x10000 + fromIntegral a3)

-- Inform 6 only. I don't know the most reliable way of figuring out what a class is, but
-- for now I assume it's a class iff it has object number <= 4 or its parent is object 1.
guessClasses objs =
  [(obj, n<=4 || p==1) | obj@(_,_,p,_,_) <- objs | n <- [1..]]

objectTableDecode'' ((attrs,propAddr,parent,sibling,child),isClass) =
  evalFrom propAddr helper where
  helper =
    do obj <- propertyTableDecode isClass attrs
       pos <- getPos
       return ((obj,parent,sibling,child),pos)


notContigError pos propAddr =
  error ("Object table not contiguous: next pointer is 0x"
   ++ showHex propAddr (", but expected 0x" ++ showHex pos ""))


propertyTableDecode | isInform6  = propertyTableDecodeInform6
                    | otherwise  = propertyTableDecodeNormal

propertyTableDecodeInform6 False attrs =
  do name   <- getObjectName
     cprops <- getProperties
     let props = getIndivProps cprops
     return (name, attrs, props)

-- Inform 6 class objects have false (empty) attribute information in
-- the usual location, with the real info stored after the property
-- list. Prior to 6.12 the attributes were not moved.
propertyTableDecodeInform6 True attrs =
  do name   <- getObjectName
     getProperties	-- ignore this property list
     attrs  <- if hdrInformVersion < 612 then return attrs else getAttrs
     cprops <- getProperties
     let props = getIndivProps cprops
     return (name, attrs, props)

propertyTableDecodeNormal _ attrs = do
  name   <- getObjectName
  props  <- getProperties
  return (name, attrs, props)

getObjectName =
  do len  <- getUByte
     name <- getBytes (len*2)
     return (evalState getZString name)

getProperties = liftM reverse (repeatUntilNothing propertyDecode)

propertyDecode = do
  sizeByte   <- getUByte
  case sizeByte of
    0 -> return Nothing
    _ -> do (num,size) <- getPropNumSize sizeByte
            bytes      <- getBytes size
            return (Just (num,PropValBlock size bytes ""))

getPropNumSize | oldObjectFormat = getPropNumSizeOld
               | otherwise       = getPropNumSizeNew

getPropNumSizeOld s =
  return (s .&. 31, (s `shiftR` 5) + 1)

getPropNumSizeNew s
  | testBit s 7  = do t <- getUByte
                      let size = case t .&. 63 of
                                   0 -> 64
                                   n -> n
                      return (num,size)
  | testBit s 6  = return (num, 2)
  | otherwise    = return (num, 1)
  where num = s .&. 63

repeatUntilNothing act =
  do x <- act
     case x of
       Just y  -> do ys <- repeatUntilNothing act
                     return (y:ys)
       Nothing -> return []

getIndivProps props =
  case prop3 of
    [(3,PropValBlock 2 bytes _)]
      -> otherProps ++ evalFrom (evalState getUWord bytes)
                                (repeatUntilNothing getIndivProp)
    _ -> props
  where (prop3, otherProps) = partition ((3==).fst) props

-- FIXME: save privacy info (private prop. if bit 15 set)
getIndivProp =
  do n     <- getUWord
     if n == 0 then return Nothing else do
     len   <- getUByte
     bytes <- getBytes len
     return $ Just (n .&. 32767, PropValBlock len bytes "")


{-----------}


objectList = map decodeObject rawObjectList

decodeObject ((name,attribs,props),parent,sibling,child) =
  ((name,attribs,map decodeProp props),parent,sibling,child)

decodeProp (n, PropValBlock size bytes _) =
  (n,decodePropType (propType n) size bytes)

decodePropType PropTypeUnknown size bytes =
  PropValBlock size bytes ""

decodePropType PropTypeExit size bytes
  | ver [1..3]  = decodeExitV3 size bytes
  | otherwise   = decodeExitV5 size bytes

decodePropType PropTypeBZExit 2 bytes =
  case decodeBZExit (evalState getUWord bytes) of
    Nothing -> PropValBlock 2 bytes "\t! invalid exit info"
    Just x  -> x

decodePropType PropTypeBZExit size bytes =
  PropValBlock size bytes "\t! expected two-byte address of exit info"

decodePropType (PropTypeSingle t) size bytes
  | size == 1  = PropValList [(t,evalState getUByte bytes)]
  | size == 2  = PropValList [(t,evalState getUWord bytes)]
  | otherwise  = PropValBlock size bytes "\t! expected one- or two-byte value"

decodePropType (PropTypeMulti (ArrayInfo _ init loop)) size bytes =
  if extra /= 0 then
    PropValBlock size bytes "\t! byte count does not match type"
  else
    PropValList $ evalState
      (mapM getArrayElem (concat (init : replicate loops loop))) bytes
  where
    (loops,extra) =
      (size - sum (map fst init)) `divMod` (sum (map fst loop))
    getArrayElem (b,t) =
      do -- pos <- getPos
         val <- if b==1 then getUByte else getUWord
         return (t,val)


decodeExitV3 1 bytes =
  PropValList [(TypeObject, evalState getUByte bytes)]

decodeExitV3 2 bytes =
  PropValList [(TypeStringPaddr, evalState getUWord bytes)]

decodeExitV3 3 bytes =
  PropValList [(TypeRoutinePtr TypeObject [], evalState getUWord bytes)]

decodeExitV3 4 bytes =
  flip evalState bytes $
    do obj  <- getUByte
       glob <- getUByte
       str  <- getUWord
       if glob >= 16
         then return (PropValCEXIT obj (glob-16) str)
         else return (noExit 4 bytes)

decodeExitV3 5 bytes =
  flip evalState bytes $
    do obj1 <- getUByte
       obj2 <- getUByte
       str  <- getUWord
       return (PropValDEXIT obj1 obj2 str)

decodeExitV3 n bytes =
  noExit n bytes

decodeExitV5 1 bytes =
  PropValList [(TypeObject, evalState getUByte bytes)]

decodeExitV5 2 bytes =
  PropValList [(TypeObject, evalState getUWord bytes)]

decodeExitV5 3 bytes =
  PropValList [(TypeStringPaddr, evalState getUWord bytes)]

decodeExitV5 4 bytes =
  PropValList [(TypeRoutinePtr TypeObject [], evalState getUWord bytes)]

decodeExitV5 5 bytes =
  flip evalState bytes $
    do obj  <- getUByte
       str  <- getUWord
       glob <- getUByte
       if glob >= 16
         then return (PropValCEXIT obj (glob-16) str)
         else return (noExit 5 bytes)

decodeExitV5 6 bytes =
  flip evalState bytes $
    do obj1 <- getUWord
       obj2 <- getUWord
       str  <- getUWord
       return (PropValDEXIT obj1 obj2 str)

decodeExitV5 n bytes =
  noExit n bytes


noExit n block = PropValBlock n block "\t! expected a ZIL exit value"


makeWords (a:b:xs) = a*256+b : makeWords xs
makeWords [] = []


decodeBZExit 0 = Just $ PropValList [(TypeInt,0)]
decodeBZExit addr =
  case byteAt addr of
    1 -> Just $ PropValList [(TypeInt,0)]	-- no exit
    2 -> Just $ PropValList [(TypeObject,wordAt (addr+2))]
    3 -> Just $ PropValList [(TypeStringPaddr,wordAt (addr+4)),
                             (TypeObject,wordAt (addr+2))]
    4 -> Just $ PropValList [(TypeRoutinePtr TypeObject [],wordAt (addr+2))]
    5 -> Just $ PropValDEXIT (wordAt (addr+2)) (wordAt (addr+4)) 0
    6 -> Just $ PropValList [(TypeStringPaddr,wordAt (addr+2))]
    7 -> Just $ PropValList [(TypeObject,wordAt (addr+2))]
    8 -> Just $ PropValList [(TypeInt,0)]	-- no exit
    _ -> Nothing


propType n =
  fromMaybe PropTypeUnknown (n `tableLookup` guessedPropTypes)


guessedPropTypes =
  makeLookupTable $
    --
    -- Declarations by the user take precedence.
    [(n,t) | SFProperty n _ t <- syms, knownPropType t]
    --
    -- In Infocom files, there's a "direction" type for dictionary
    -- words, which tells us which properties are ZIL exits.
    ++ [(n,PropTypeExit) | n <- dirProps]
    --
    -- Finally, look at the property contents.
    ++ [(n,t) | (n,bs) <- allProps, t <- maybeToList (guessPropTypeByContents bs)]
  where allProps =
          group $ [(n,(x,y)) | ((_,_,props),_,_,_) <- rawObjectList,
                           (n,PropValBlock x y _) <- props]


guessPropTypeByContents :: [(Int,DataBlock)] -> Maybe PropType

guessPropTypeByContents [] = Nothing

guessPropTypeByContents vals
  | any odd lengths          = Nothing
  | all isDictWord valWords  = Just (prop TypeDictWord)
  | and areRoutines          = Just (prop (TypeRoutinePtr TypeUnknown []))
  | and areStrings           = Just (prop TypeStringPaddr)
  | and areObjects           = Nothing	-- unreliable to guess object #s here
  | and (zipWith3 or3 areRoutines areStrings areObjects)
                             = Just (prop TypeThing)
  | otherwise                = Nothing
  where
    lengths      = map fst vals
    valWords     = filter (/= 65535) (concatMap getWords vals)
    areRoutines  = map isKnownRoutinePaddr valWords
    areStrings   = map isKnownStringPaddr valWords
    areObjects   = map (inRange (0,numObjects)) valWords
    or3 a b c    = a || b || c
    prop         = if all (<= 2) lengths
                     then PropTypeSingle
                     else (\t -> PropTypeMulti (ArrayInfo UnknownLength [] [(2,t)]))
    getWords (_,block) = evalState (repeatUntilEmpty getUWord) block


dirProps =
  [prop | (_,_,tags) <- elems dictionary, DictDir prop <- tags]


isInform6 =
  case compiler of
    Inform6 -> True
    _       -> False
