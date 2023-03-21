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


module Reform_symfile (
	SFDirective(..), PropType(..),
	syms, usedSymFileName,
	sfPrintFunc,
	sfRoutineType, sfLocalType, sfLocalTypes,
	sfGlobalType, sfGlobalSilent, sfArrayType,
	sfFalseEnd,
	sfFullDictWord, sfWordNamed,
	lastGlobal, compiler,
	wordTypeNames, propTypeNames, arrayTypeNames,
	knownType, knownPropType
) where


import Reform_storyfile
import Reform_zcode
import Reform_util
import Reform_cmdline

import Data.Array.IArray
import Data.Array.Unboxed
import Control.Monad (msum,mplus,fmap,liftM)
import List (sortBy)
import Maybe (isJust,fromMaybe,fromJust)
import Char (toLower,isSpace,isDigit)
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)


data SFDirective
  = SFCodeArea     !DataBlock
  | SFStringArea   !DataBlock
  | SFObjName      !Int String
  | SFAttrName     !Int String
  | SFProperty     !Int String PropType
  | SFRoutine      !Int String ZType [String] [ZType]
  | SFGlobal       !Int String ZType
  | SFGlobalSilent !Int
  | SFPrintRoutine !Int String
  | SFFalseEnd     !Int !Int
  | SFActionName   !Int String
  | SFArray        !Int String ArrayInfo
  | SFFullDictWord String String
  | SFLastGlobal   !Int
  | SFEnum         String !EnumInfo
  | SFCompiler     !ZCompiler
  | SFMD5          String


data PropType
  = PropTypeUnknown
  | PropTypeExit
  | PropTypeBZExit
  | PropTypeSingle !ZType
  | PropTypeMulti  !ArrayInfo
  deriving (Eq)


sfPrintFunc :: Int -> Maybe String
sfPrintFunc f =
  f `tableLookup` printFuncs

printFuncs =
  makeLookupTable [(a,n) | SFPrintRoutine a n <- syms]


sfRoutineType :: Int -> ZType
sfRoutineType f =
  fromMaybe TypeUnknown (f `tableLookup` routineTypes)

routineTypes =
  makeLookupTable [(a,t) | SFRoutine a _ t _ _ <- syms, knownType t]


sfLocalTypes :: Int -> [ZType]
sfLocalTypes f =
  fromMaybe [] (f `tableLookup` routineArgTypes)

sfLocalType :: Int -> Int -> ZType
sfLocalType f n =
  (sfLocalTypes f ++ repeat TypeUnknown) !! (n-1)

routineArgTypes =
  makeLookupTable [(a,ts) | SFRoutine a _ _ _ ts <- syms]


sfGlobalType :: Int -> ZType
sfGlobalType n = globalTypes ! n

globalTypes :: Array Int ZType
globalTypes =
  accumArray (\a b -> b) TypeUnknown (0,lastGlobal)
    [(n,t) | SFGlobal n _ t <- syms]


sfGlobalSilent :: Int -> Bool
sfGlobalSilent n = globalSilent ! n

globalSilent :: UArray Int Bool
globalSilent =
  accumArray (\a b -> b) False (0,lastGlobal)
    [(n,True) | SFGlobalSilent n <- syms]


sfArrayType :: Int -> Maybe ZType
sfArrayType n =
  fmap TypeArrayPtr (n `tableLookup` arrayTypes)

arrayTypes =
  makeLookupTable [(a,t) | SFArray a _ t <- syms]


sfFalseEnd :: Int -> Int
sfFalseEnd n = fromMaybe 0 (n `lookup` falseEnds)

falseEnds =
  [(addr,ends) | SFFalseEnd addr ends <- syms]


sfFullDictWord :: String -> String
sfFullDictWord w = fromMaybe w (w `tableLookup` fullDictWords)

fullDictWords =
  makeLookupTable
    [(x,y) | SFFullDictWord x y <- syms, x /= y]

sfWordNamed :: String -> Bool
sfWordNamed w = isJust (w `tableLookup` fullDictWords)


compiler :: ZCompiler
compiler = fromMaybe hdrCompiler sfCompiler

sfCompiler =
  case uniq [c | SFCompiler c <- syms] of
    []         -> Nothing
    [compiler] -> Just compiler
    _          -> error "Only one Zilch/Inform directive allowed!"


lastGlobal :: Int
lastGlobal =
  case uniq [n | SFLastGlobal n <- syms] of
    [] -> 239
    [n] | n >= 0 && n <= 239  -> n
        | otherwise           -> error "LastGlobal out of range"
    _  -> error "Conflicting LastGlobal directives"


{---------------}


syms :: [SFDirective]
usedSymFileName :: String

(syms,usedSymFileName) =
  (parseSymFile (unsafePerformIO (reader name)), name)
  where (reader,name) =
          case symFileName of
            Just name -> (readFile, name)
            Nothing   -> (maybeReadFile, guessSymFileName storyFileName)


guessSymFileName base =
  reverse (dropWhile ('.' /=) (reverse base)) ++ "reform"

maybeReadFile name =
  catch (readFile name) (\_ -> return "")


{---------------}


parseSymFile s =
  concat [parseSymFileLine n (myWords l)
           | l <- lines s | n <- [1..]]

parseSymFileLine n [] = []

parseSymFileLine n (directive:args) =
  case (map toLower directive) `lookup` directives of
    Nothing -> error ("Unrecognized directive \"" ++ directive ++ "\" on symbol file line " ++ show n)
    Just (chkArgc, func) ->
      if not (chkArgc (length args)) then
        error ("Wrong number of arguments to directive \"" ++ directive ++ "\" on symbol file line " ++ show n)
      else
        func [(s, myRead n s) | s <- args]

myRead :: Int -> String -> Int

myRead line str =
  case reads str of
    [(int,"")] -> int
    _ -> error ("Expected a number, got \"" ++ str ++ "\" on symbol file line " ++ show line)


myWords :: String -> [String]
myWords s =
  case dropWhile isSpace s of
    ""         -> []
    ('!':_)    -> []
    ('"':rest) -> case break ('"'==) rest of
                    (w,"")         -> [w]
                    (w,('"':rest)) -> w : myWords rest
    rest       -> w : myWords x where (w,x) = break isSpace rest


directives =
 [("md5",	((==1),	\[(x,_)] -> [SFMD5 x])),
  ("codearea",	(two,	area SFCodeArea)),
  ("stringarea",(two,	area SFStringArea)),
  ("object",	(two,	name SFObjName)),
  ("attribute",	(two,	name SFAttrName)),
  ("property",	(two,	property)),
  ("routine",	((>=2),	routine)),
  ("global",	(two,	global)),
  ("array",     (two,	array_)),
  ("globalarray",(two,	globalArray)),
  ("action",	(two,	name SFActionName)),
  ("word",	(two,	\[(x,_),(y,_)] -> [SFFullDictWord x y])),
  ("printroutine",(two,	name SFPrintRoutine)),
  ("falseend",	(two,	\[(_,addr),(_,ends)] -> [SFFalseEnd addr ends])),
  ("lastglobal",((==1),	\[(_,n)] -> [SFLastGlobal n])),
  ("enum",	((>=2),	enum)),
  ("zilch",	(zero,	\_ -> [SFCompiler Zilch])),
  ("inform5",	(zero,	\_ -> [SFCompiler Inform5])),
  ("inform6",	(zero,	\_ -> [SFCompiler Inform6]))]

zero = (0==)
two  = (2==)


{---------------}


area cons [(_,from),(_,to)] = [cons (from,to)]
name cons [(_,n),(ident,_)] = [cons n ident]

routine ((_,addr) : (id,_) : args) =
  let (n,t) = parseTypeTag parseWordType id
      (ns,ts) = unzip (map (parseTypeTag parseWordType . fst) args)
  in [SFRoutine addr n t ns ts]

property [(_,num), (id,_)] =
  let (name,t) = parseTypeTag parsePropType id
  in [SFProperty num name t]

global [(_,num), (id,_)] =
  [SFGlobal num name t]
  where (name,t) = parseTypeTag parseWordType id

array_ [(_,addr), (id,_)] =
  [uncurry (SFArray addr) (parseTypeTag parseArrayType id)]

globalArray [(_,num), (id,_)] =
  [SFGlobalSilent num,
   SFGlobal num name (TypeArrayPtr t),
   SFArray (wordAt (hdrGlobalVarTableAddr + num*2)) name t]
  where (name,t) = parseTypeTag parseArrayType id

enum ((name,_) : valstrings) =
  [SFEnum name info]
  where
    info =
      case sortFst (map parseVal valstrings) of
        ((0,z) : vals) -> EnumInfo (reverse vals) z
        vals           -> EnumInfo (reverse vals) "0"
    parseVal (s,_) =
      case break (=='=') s of
        (_,"") -> error "missing \"=\" in Enum value"
        (name,_:rest) ->
          case reads rest of
            [(num,"")] -> (num,name)
            _          -> error ("expected a number, got " ++ show rest ++ " in Enum value")


parseTypeTag :: (TypeStruct -> Maybe a) -> String -> (String,a)

parseTypeTag lookupType id =
  case break (':' ==) id of
    (name,"")      -> (name, fromJust (lookupType (TypeLit "?")))
    (name,(_:t))   ->
      case lookupType (parseTypeStruct (map toLower t)) of
        Just t  -> (name,t)
        Nothing -> error ("Unrecognized type signature: \"" ++ id ++ "\"")


parseWordType (TypePtr s) =
  mplus (parseRoutineType s)
        (liftM TypeArrayPtr (parseArrayType s))

parseWordType (TypeLit s) =
  (s `lookup` enums) `mplus` (s `lookup` wordTypeNames)

parseWordType _ = Nothing


enums =
  [(map toLower name,TypeEnum info) | SFEnum name info <- syms]


parsePropType :: TypeStruct -> Maybe PropType

parsePropType s =
  msum [parsePropTypeBasic s,
        fmap PropTypeMulti (parseArrayType s),
        fmap PropTypeSingle (parseWordType s)]

parsePropTypeBasic (TypeLit s) = s `lookup` propTypeNames
parsePropTypeBasic _ = Nothing


parseArrayType :: TypeStruct -> Maybe ArrayInfo

parseArrayType (TypeLit l) = l `lookup` arrayTypeNames

parseArrayType (TypeFunc id x) =
  case id of
    "array"  -> helper x
    "table"  -> helper [TypeLit "int", TypeMult StoredLength x]
  where
    helper xs =
      case last xs of
        TypeMult len rest ->
          do start <- mapM helper' (init xs)
             loop <- mapM helper' rest
             return (ArrayInfo len start loop)
        _                 ->
          do loop <- mapM helper' xs
             return (ArrayInfo (ConstLength 1) [] loop)
    helper' (TypeHalf t) = liftM ((,) 1) (parseWordType t)
    helper' t            = liftM ((,) 2) (parseWordType t)

parseArrayType _ = Nothing


parseRoutineType :: TypeStruct -> Maybe ZType

parseRoutineType (TypeLit "routine") =
  Just anonRoutine

parseRoutineType (TypeFunc "routine" ts) =
  case mapM parseWordType ts of
    Just (t:ts) -> Just (TypeRoutinePtr t ts)
    Just []     -> Just anonRoutine
    Nothing     -> Nothing

parseRoutineType _ = Nothing

anonRoutine = TypeRoutinePtr TypeUnknown []


wordTypeNames =
 [("?",		TypeUnknown),
  ("object",	TypeObject),
  ("property",	TypeProp),
  ("attribute",	TypeAttr False),
  ("attribute0",TypeAttr True),
  ("routine",	TypeRoutinePtr TypeUnknown []),
  ("string",	TypeStringPaddr),
  ("char",	TypeZsciiChar),
  ("unicode",	TypeUnicodeChar),
  ("int",	TypeInt),
  ("bool",	TypeBool),
  ("dictword",	TypeDictWord),
  ("action",	TypeAction),
  ("adjective",	TypeAdjectiveNum),
  ("verbnum",	TypeVerbNum),
  ("zerotable",	TypeUnknown),	-- FIXME
  ("thing",	TypeThing)]

propTypeNames =
 [("?",		PropTypeUnknown),
  ("exit",	PropTypeExit),
  ("bzexit",	PropTypeBZExit)]

arrayTypeNames =
 [("objbytes",	byteArray TypeObject),
  ("objwords",	wordArray [TypeObject]),
  ("things",	wordArray [TypeThing]),
  ("routines",	wordArray [TypeRoutinePtr TypeUnknown []]),
  ("strings",	wordArray [TypeStringPaddr]),
  ("dictwords",	wordArray [TypeDictWord]),
  ("adjbytes",	byteArray TypeAdjectiveNum),
  ("pseudo",	wordArray [TypeDictWord,TypeRoutinePtr TypeUnknown []]),
  ("array",	wordArray [TypeUnknown]),
  ("barray",	byteArray TypeUnknown)]
 where
   wordArray types = ArrayInfo UnknownLength [] (map ((,)2) types)
   byteArray typ   = ArrayInfo UnknownLength [] [(1,typ)]


knownType TypeUnknown = False
knownType _           = True

knownPropType PropTypeUnknown = False
knownPropType _               = True


{---------------}


data TypeStruct
  = TypeLit String
  | TypeFunc String [TypeStruct]
  | TypeMult ArrayLengthInfo [TypeStruct]
  | TypePtr TypeStruct
  | TypeHalf TypeStruct


parseTypeStruct s =
  let ?err = ("bad type syntax: " ++ s) in
  case parseTypeStruct' s of
    (result,"") -> result
    _           -> error ?err

parseTypeStruct' ('^' : cs) =
  onFst TypePtr (parseTypeStruct' cs)

parseTypeStruct' ('~' : cs) =
  onFst TypeHalf (parseTypeStruct' cs)

parseTypeStruct' ('*' : cs) =
  onFst (TypeMult UnknownLength) (parseTypeListOrLit cs)

parseTypeStruct' ('n' : '*' : cs) =
  onFst (TypeMult StoredLength) (parseTypeListOrLit cs)

parseTypeStruct' cs@(c:_) | isDigit c =
  case reads cs of
    [(n,'*':rest)] -> onFst (TypeMult (ConstLength n)) (parseTypeListOrLit rest)
    _              -> error ?err

parseTypeStruct' cs =
  case break (`elem` "(,)") cs of
    ("",_)        -> error ?err
    (id,'(':')':rest) -> (TypeFunc id [],rest)
    (id,'(':rest) -> onFst (TypeFunc id) (parseTypeList2 rest)
    (id,rest)     -> (TypeLit id,rest)

parseTypeListOrLit ('(' : rest) =
  parseTypeList2 rest

parseTypeListOrLit rest =
  ([x],rest') where (x,rest') = parseTypeStruct' rest

parseTypeList2 cs =
  case parseTypeList cs of
    (list1,')':rest) -> (list1,rest)
    _                -> error ?err

parseTypeList cs =
  case parseTypeStruct' cs of
    (id,',':rest) -> onFst (id:) (parseTypeList rest)
    (id,rest)     -> ([id],rest)
