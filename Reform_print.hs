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
{-# LANGUAGE ImplicitParams, ParallelListComp #-}

module Reform_print (
    ppRoutine, ppObject, ppVerbs,
    ppString, ppGlobal, ppArray,
    ppDictEntry,
    outputSymFile
) where


import Reform_zcode
import Reform_zscii
import Reform_util
import Reform_cmdline
import Reform_storyfile
import Reform_symfile
import Reform_objects
import Reform_highmem
import Reform_grammar
import Reform_names
import Reform_inference

import Control.Monad (replicateM,liftM,liftM2)
import Control.Monad.State (evalState)
import Data.Array
import Data.Ix (inRange,range)
import Data.Maybe (fromMaybe,listToMaybe,mapMaybe,isJust,isNothing)
import Numeric (showHex,showIntAtBase)
import Data.Char (ord,chr,toLower,isAlphaNum)
--import Data.PackedString (packString,indexPS)
--import qualified Data.ByteString as BSTR
import Data.Bits
import Data.List (sort,nub)

import Debug.Trace (trace)


ppRoutine :: Int -> [Int] -> [ZInstr] -> [String]

ppRoutine addr params code =
  let ?nameLocal = nameLocal addr in
    let firstLine =
          "[ " ++ nameRoutine addr
           ++ concat [' ' : nameParam n v t | n <- [1..] | v <- params | t <- sfLocalTypes addr ++ repeat TypeUnknown]
           ++ " ;" ++ showDecAndHex addr
        code' = inferStmtTypes (sfRoutineType addr) (sfLocalType addr) code
    in ("" : ppRoutineRefs addr [""] ++ firstLine
           : indentBlock (ppInstrs code') ++ ["];"])

nameParam n 0 t = ?nameLocal n
nameParam n v t = ?nameLocal n ++ '=' : ppConst' t v


ppRoutineRefs addr blank =
  case tableLookup addr routineRefs of
    Nothing   -> []
    Just refs -> ("! References: " ++ join ", " (map helper refs)) : blank
  where helper (obj,prop) = nameObject obj ++ '.' : nameProp prop

routineRefs =
  makeLookupTable $ group $ sortFst
    [(unpackRoutineAddr paddr,(obj,prop)) |
          (obj,(_,_,props)) <- assocs objects,
          (prop,PropValList vals) <- props,
          (_,paddr) <- vals]


ppInstrs x@(ZEval c@(Call _ _):_) | isPrintRoutine c     = ppPrint x
ppInstrs x@(ZPush c@(Call _ _):_) | isPrintRoutine c     = ppPrint x
ppInstrs x@(ZReturn c@(Call (Prim PrimPrintInline) _):_) = ppPrint x
ppInstrs (x:xs) = ppInstr x ++ ppInstrs xs
ppInstrs [] = []


ppInstrsEmpty [] = ["! no code"]
ppInstrsEmpty [ZExitLabels _] = ["! no code"]
ppInstrsEmpty instrs = ppInstrs instrs


ppInstr (ZExitLabels _) = []
ppInstr (ZLabel ad _) = ["<." ++ ppLabel ad ++ ";"]
ppInstr (ZEval x@(IfThenExpr pred yes no)) = ppInstr (cvtIfThen ZEval x)
ppInstr (ZEval expr) = [ppExpr 0 expr ++ ";"]
ppInstr (ZPush x@(IfThenExpr pred yes no)) = ppInstr (cvtIfThen ZPush x)
ppInstr (ZPush expr) = [ppExpr 0 expr ++ ";\t! not popped"]
ppInstr (ZReturn (Const _ TypeBool 0)) = ["rfalse;"]
ppInstr (ZReturn (Const _ TypeBool 1)) = ["rtrue;"]
ppInstr (ZReturn expr) = ["return " ++ ppExpr 0 expr ++ ";"]
ppInstr (ZJump label) = [ppJump label ++ ";"]
ppInstr (ZJCond expr label) = ["if (" ++ ppExpr 0 expr ++ ") " ++ ppJump label ++ ";"]
ppInstr foo@(ZIfThenElse _ _ _) =
  let (conds,last) = getConds [foo] in
  concat $ zipWith ppIf ("" : repeat "} else ") conds ++ ppElse last : [["}"]]
-- FIXME: how to show indir?
ppInstr (ZInstr name _ indir args store branch) =
  ['@' : unwords (name : map (ppExpr 99) args ++ storeArg ++ branchArg) ++ ";"]
  where
    storeArg  = maybe [] (\dest -> ["->", ppExpr 99 dest]) store
    branchArg = maybe [] (\(dir,dest) -> ['?' : (if dir then "" else "~") ++ ppLabel dest]) branch
ppInstr (ZSpecial str) = [str ++ ";"]
ppInstr ZNop = []


cvtIfThen last (IfThenExpr pred yes no) =
  ZIfThenElse pred (helper yes) (helper no)
  where
    helper [x] = [last x]
    helper (x:xs) = (ZEval x : helper xs)


getConds (ZIfThenElse pred yes no : foo) | isBlockExit foo =
  onFst ((pred,yes):) (getConds no)

getConds (ZPush x@(IfThenExpr _ _ _) : foo) | isBlockExit foo =
  getConds [cvtIfThen ZPush x]

getConds (ZEval x@(IfThenExpr _ _ _) : foo) | isBlockExit foo =
  getConds [cvtIfThen ZEval x]

getConds x = ([],x)

isBlockExit [] = True
isBlockExit [ZExitLabels _] = True
isBlockExit _ = False


ppIf prefix (cond,body) =
  (prefix ++ "if (" ++ ppExpr 0 cond ++ ") {")
   : indentBlock (ppInstrsEmpty body)

ppElse [] = []
ppElse body = "} else {" : indentBlock (ppInstrs body)


{---------------}


ppPrint x = getPrints id x

getPrints sofar (ZEval c@(Call (Prim p) xs) : rest) | isPrintRoutine c =
  getPrints (sofar . ((p,xs):)) rest
getPrints sofar (ZPush c@(Call (Prim p) xs) : rest) | isPrintRoutine c =
  getPrints (sofar . ((p,xs):)) rest
getPrints sofar (ZReturn (Call (Prim p@PrimPrintInline) xs) : rest) =
  ppPrint' True (sofar [(p,xs)]) : ppInstrs rest
getPrints sofar rest =
  ppPrint' False (sofar []) : ppInstrs rest

ppPrint' ret vals =
  keyword ++ join ", " (map (uncurry helper) vals) ++ ";"
  where
    keyword = if ret then "" else "print "
    helper PrimPrintInline [k] = ppExpr 1 k
    helper PrimPrintAddr   [x] = "(address) " ++ ppExpr 1 x
    helper PrimPrintPaddr  [x] = printPrim "" "(string) " x
    helper PrimPrintObj    [x] = printPrim "(name) " "(name) " x
    helper PrimPrintChar   [x] = printPrim "" "(char) " x
    helper PrimPrintNum    [x] = printPrim "" "" x
    helper PrimInvoke [Const _ _ f, x] =
      '(' : namePrintFunc f ++ ") " ++ ppExpr 1 x

printPrim constPrefix prefix k@(Const _ _ _) =
  constPrefix ++ ppExpr 1 k
printPrim constPrefix prefix x =
  prefix ++ ppExpr 1 x


isPrintRoutine (Call (Prim PrimPrintInline) [_]) = True
isPrintRoutine (Call (Prim PrimPrintAddr) [_])   = True
isPrintRoutine (Call (Prim PrimPrintPaddr) [_])  = True
isPrintRoutine (Call (Prim PrimPrintObj) [_])    = True
isPrintRoutine (Call (Prim PrimPrintChar) [_])   = True
isPrintRoutine (Call (Prim PrimPrintNum) [_])    = True
isPrintRoutine (Call (Prim PrimInvoke) [Const _ _ f, _]) =
  isJust (sfPrintFunc (unpackRoutineAddr f))
isPrintRoutine _ = False


{---------------}


ppExpr :: (?nameLocal :: Int -> String) => Int -> Expr -> String

ppExpr _ SP = "SP"
ppExpr _ (Local n)  = ?nameLocal n
ppExpr _ (Global n) = nameGlobal n

ppExpr _ k@(Const _ _ _) = ppConst k

ppExpr prec (Call f args) = ppCall prec f args

ppExpr prec (Assignment dst src) =
  parenIf (1 < prec) (ppAssignment dst src)

-- FIXME?: Inform doesn't actually have ?:
-- parenthesize defensively
ppExpr prec (IfThenExpr pred yes no) =
  '(' : ppExpr 99 pred ++ " ? " ++ ppExprs yes ++ " : " ++ ppExprs no ++ ")"

ppExprs [x] = ppExpr 99 x
ppExprs xs = '(' : join ", " (map (ppExpr 1) xs) ++ ")"


{---------------}


ppConst (Const pos typ n) =
  if showConstAddrs && pos /= 0 then
    ppConst' typ n ++ "[[" ++ showHex pos "]]"
  else
    ppConst' typ n


ppConst' TypeUnknown 0 = "0"
ppConst' TypeUnknown n =
  show n ++ '[' : guess ++ "?]"
  where
    guess
      | inRange (3,numObjects) n  = ppConst' TypeObject n
      | isDictWord n              = ppConst' TypeDictWord n
      | isKnownRoutinePaddr n     = ppConst' (TypeRoutinePtr TypeUnknown []) n
      | isKnownStringPaddr n      =
          ppQuotedString (abridge (evalFrom (unpackStringAddr n) getZString))
      | otherwise                 = ""
    abridge s =
      case splitAt 8 s of
        (a,"") -> a
        (a,_)  -> a ++ "..."

ppConst' TypeInt n       = show n

ppConst' (TypeEnum (EnumInfo _ z)) 0    = z
ppConst' (TypeEnum (EnumInfo vals _)) n =
  ppEnum vals n

ppConst' (TypeRoutinePtr _ _) 0 = "0"
ppConst' (TypeRoutinePtr _ _) n = nameRoutine (unpackRoutineAddr n)

ppConst' TypeObject 0  = "nothing"
ppConst' TypeObject n  = nameObject n

ppConst' TypeProp 0  = "0"
ppConst' TypeProp n  = nameProp n

ppConst' (TypeAttr True) 0 = "0"
ppConst' (TypeAttr _)    n = nameAttr n

ppConst' TypeAction n    = '#':'#':nameAction n

ppConst' TypeDictWord 0  = "0"
ppConst' TypeDictWord n  = ppDictWord (dictWordAt n)

ppConst' (TypeArrayPtr _) 0 = "0"
ppConst' (TypeArrayPtr _) n = fromMaybe ("invalidArray" ++ show n) (nameArray n)

ppConst' TypeZsciiChar n =
  '\'' : ppString' [zsciiCharToUnicode n] ++ "'"

ppConst' TypeStringAddr n
  | n < 64    = show n
  | otherwise = ppStringAt n

ppConst' TypeStringPaddr n
  | n' < 64   = show n
  | otherwise = ppStringAt n'
  where n' = unpackStringAddr n

ppConst' TypeBool 0      = "false"
ppConst' TypeBool 1      = "true"
ppConst' TypeBool n      = "invalidBool" ++ show n

ppConst' TypeAdjectiveNum 0 = "0"
ppConst' TypeAdjectiveNum n =
  case getInfocomAdjectiveNum n of
    Just pos -> ppConst' TypeDictWord pos
    Nothing  -> "invalidAdjective" ++ show n

ppConst' TypeVerbNum 0 = "0"
ppConst' TypeVerbNum n =
  case verbNums!n of
    []     -> "invalidVerb" ++ show n
    (v:vs) -> "#v$" ++ v

ppConst' TypeThing n
  | n == 0     = "nothing"  -- could also be rfalse routine
  | n == 65535 = "NULL"     -- FIXME: ???
  | n <= numObjects  = nameObject n
  | otherwise  =
      let knownRoutine = isKnownRoutinePaddr n
          knownString  = isKnownStringPaddr n
      in if knownRoutine && not knownString then
           ppConst' (TypeRoutinePtr TypeUnknown []) n
         else if knownString && not knownRoutine then
           ppConst' TypeStringPaddr n
         else
           "invalidThing" ++ show n

ppStringAt n = ppQuotedString (evalFrom n getZString)


{---------------}


ppEnum vals n =
  case helper vals n of
    [x] -> x
    xs  -> '(' : join "|" xs ++ ")"
  where
    helper [] 0 = []
    helper [] n = ["invalidEnum" ++ show n]
    helper ((val,name):vals) n
      | n .&. val == val  = name : helper vals (n `xor` val)
      | otherwise         = helper vals n


{---------------}


ppCall ::
  (?nameLocal :: Int -> String) =>
    Int -> FuncType -> [Expr] -> String

ppCall prec (Prim p) args =
  (primInfo!p) prec ppExpr args

ppCall prec (PrimJump p b) args =
  (primJumpInfo!p) b prec ppExpr args


binaryLeft2 name prec' prec ppExpr [x,y] =
  parenIf (prec' < prec) $
    ppExpr prec' x ++ name ++ ppExpr (prec'+1) y

binaryDot prec ppExpr [x,y] =
  parenIf (12 < prec) $
    ppExpr 11 x ++ '.' : ppExpr 13 y

binaryOr name prec ppExpr (x:ys) =
  parenIf (3 < prec) $
    ppExpr 3 x ++ name ++ join " or " (map (ppExpr 4) ys)

unary name precOut precIn prec ppExpr [x] =
  parenIf (precOut < prec) $
    name ++ ppExpr precIn x

unaryPost name precOut precIn prec ppExpr [x] =
  parenIf (precOut < prec) $
    ppExpr precIn x ++ name

specialFunc name prec ppExpr args =
  parenIf (11 < prec) $
    name ++ '(' : join "," (map (ppExpr 1) args) ++ ")"

normalFunc prec ppExpr (func:args) =
  parenIf (11 < prec) $
    ppExpr 11 func ++ '(' : join "," (map (ppExpr 1) args) ++ ")"

specialSyntax1 a prec ppExpr [x] =
  a ++ ppExpr 0 x

specialSyntax2 a b prec ppExpr [x,y] =
  a ++ ppExpr 0 x ++ b ++ ppExpr 0 y

ppSetTextStyle _ _ [Const _ _ 0] = "style roman"
ppSetTextStyle _ _ [Const _ _ 1] = "style reverse"
ppSetTextStyle _ _ [Const _ _ 2] = "style bold"
ppSetTextStyle _ _ [Const _ _ 4] = "style underline"
ppSetTextStyle _ ppExpr [x] = "@set_text_style " ++ ppExpr 99 x

primInfo :: Array Prim (Int -> (Int -> Expr -> String) -> [Expr] -> String)
primInfo =
  array (minBound,maxBound)
  [(PrimInvoke,     normalFunc),
   (PrimAnd,        binaryLeft2 " && " 2),
   (PrimOr,     binaryLeft2 " || " 2),
   (PrimGetProp,    binaryDot),
   (PrimGetPropAddr,    binaryLeft2 ".&" 10),
   (PrimGetPropLen, specialFunc "get_prop_len"),
   (PrimPutProp,    \prec ppExpr [x,y,z] -> ppExpr prec (Assignment (Call (Prim PrimGetProp) [x,y]) z)),
   (PrimPlus,       binaryLeft2 " + " 5),
   (PrimMinus,      binaryLeft2 " - " 5),
   (PrimTimes,      binaryLeft2 " * " 6),
   (PrimDivide,     binaryLeft2 " / " 6),
   (PrimMod,        binaryLeft2 " % " 6),
   (PrimBitOr,      binaryLeft2 " | " 6),
   (PrimBitAnd,     binaryLeft2 " & " 6),
   (PrimBitNot,     unary "~" 5 7),
   (PrimLogNot,     unary "~~" 1 3),
   (PrimLoadB,      binaryLeft2 "->" 7),
   (PrimLoadW,      binaryLeft2 "-->" 7),
   (PrimStoreB,     \prec ppExpr [x,y,z] -> ppExpr prec (Assignment (Call (Prim PrimLoadB) [x,y]) z)),
   (PrimStoreW,     \prec ppExpr [x,y,z] -> ppExpr prec (Assignment (Call (Prim PrimLoadW) [x,y]) z)),
   (PrimGetParent,  specialFunc "parent"),
   (PrimGetChild,   specialFunc "child"),
   (PrimGetSibling, specialFunc "sibling"),
   (PrimMove,       specialSyntax2 "move " " to "),
   (PrimRemove,     specialSyntax1 "remove "),
   (PrimGetNextProp,    specialFunc "get_next_prop"),
   (PrimFSet,       specialSyntax2 "give " " "),
   (PrimFClear,     specialSyntax2 "give " " ~"),
   (PrimRandom,     specialFunc "random"),
   (PrimSetTextStyle,   ppSetTextStyle),
   (PrimXPreInc,    unary "++" 8 10),
   (PrimXPostInc,   unaryPost "++" 8 10),
   (PrimXPreDec,    unary "--" 8 10),
   (PrimXPostDec,   unaryPost "--" 8 10),
   (PrimNewline,    \_ _ _ -> "new_line")]

primJumpInfo ::
  Array PrimJump
    (Bool -> Int -> (Int -> Expr -> String) -> [Expr] -> String)

primJumpInfo =
  array (minBound,maxBound)
  [(PrimIn, primJumpNormal " in "  " notin "),
   (PrimHas,    primJumpNormal " has " " hasnt "),
   (PrimEq, primJumpNormal " == "  " ~= "),
   (PrimLt, primJumpNormal " < "   " >= "),
   (PrimGt, primJumpNormal " > "   " <= "),
   (PrimEq0,    \b prec ppExpr [x] -> ppExpr prec (if b then Call (Prim PrimLogNot) [x] else x)),
   (PrimBitTest,\b prec ppExpr [x,y] -> ppExpr prec (Call (PrimJump PrimEq b) [Call (Prim PrimBitAnd) [x,y], y])),
   (PrimXDecChk,\b prec ppExpr [x,y] -> ppExpr prec (Call (PrimJump PrimLt b) [Call (Prim PrimXPreDec) [x], y])),
   (PrimXIncChk,\b prec ppExpr [x,y] -> ppExpr prec (Call (PrimJump PrimGt b) [Call (Prim PrimXPreInc) [x], y]))]

primJumpNormal x y =
  \b -> binaryOr (if b then x else y)


ppAssignment dst src =
  ppExpr 2 dst ++ " = " ++ ppExpr 1 src


{---------------}


ppObject treeDepth n (name,attribs,props) =
  "" : firstLine : indentWith (concatMap ppProp props)
     ++ ["  has\t" ++ ppAttribs attribs ++ ";"]
  where
    firstLine = "Object" ++ take (3*treeDepth+1) (cycle " ->")
                 ++ nameObject n ++ ' ' : ppQuotedString name
                 ++ showDecAndHex n


ppProp (n, PropValBlock _ block comment) =
  [nameProp n ++ " [" ++ join " " (map (\x -> showHex x "") bytes) ++ "]," ++ comment]
  where bytes = evalState (repeatUntilEmpty getUByte) block

ppProp (n, PropValList vals) =
  [join " " (nameProp n : map (\(t,v) -> ppConst' t v) vals) ++ ","]

ppProp (n, PropValCEXIT obj global string) =
  [nameProp n ++ " (TO " ++ nameObject obj ++ " IF "
    ++ nameGlobal global ++ ppELSE string ++ "),"]

ppProp (n, PropValDEXIT obj1 obj2 string) =
  [nameProp n ++ " (TO " ++ nameObject obj1 ++ " IF "
    ++ nameObject obj2 ++ " IS OPEN" ++ ppELSE string ++ "),"]


ppELSE 0      = ""
ppELSE string = " ELSE " ++ ppConst' TypeStringPaddr string


ppAttribs attribs =
  join " " [nameAttr attr | attr <- [0..47], testBit attribs (47-attr)]

indentWith [] = []
indentWith (firstLine : lines) =
  ("  with\t" ++ firstLine) : map ('\t' :) lines


{---------------}


ppQuotedString s = '"' : ppString s ++ "\""

ppString "" = ""
ppString (x : rest)
  | x == '\13'   = '^' : ppString rest
  | x == '"'     = '~' : ppString rest
  | x <= '\x153' = (informEscapes ! x) ++ ppString rest
  | otherwise    = informEscapeChar x (ppString rest)

ppString' "" = ""
ppString' (x : rest)
  | x == '\''    = '^' : ppString' rest
  | x == '"'     = '"' : ppString' rest
  | x <= '\x153' = (informEscapes ! x) ++ ppString' rest
  | otherwise    = informEscapeChar x (ppString' rest)


-- correct behavior of showHex requires GHC 6.2
informEscapeChar x rest =
  "@{" ++ showHex (ord x) ('}' : rest)


informEscapes :: Array Char String

informEscapes =
  accumArray (\a b -> b) undefined ('\0','\x153') $
             [(x,[x]) | x <- ['\32'..'\126']]
              ++ [(x,informEscapeChar x "") | x <- ['\0'..'\31'] ++ "^~@\"" ++ ['\127'..'\x153']]
              ++ informSpecialEscapes

informSpecialEscapes :: [(Char,String)]
informSpecialEscapes =
 [('\xE4',"@:a"), ('\xF6',"@:o"), ('\xFC',"@:u"),
  ('\xC4',"@:A"), ('\xD6',"@:O"), ('\xDC',"@:U"),
  ('\xDF',"@ss"), ('\xAB',"@>>"), ('\xBB',"@<<"),
  ('\xEB',"@:e"), ('\xEF',"@:i"), ('\xFF',"@:y"),
  ('\xCB',"@:E"), ('\xCF',"@:I"), ('\xE1',"@'a"),
  ('\xE9',"@'e"), ('\xED',"@'i"), ('\xF3',"@'o"),
  ('\xFA',"@'u"), ('\xFD',"@'y"), ('\xC1',"@'A"),
  ('\xC9',"@'E"), ('\xCD',"@'I"), ('\xD3',"@'O"),
  ('\xDA',"@'U"), ('\xDD',"@'Y"), ('\xE0',"@`a"),
  ('\xE8',"@`e"), ('\xEC',"@`i"), ('\xF2',"@`o"),
  ('\xF9',"@`u"), ('\xC0',"@`A"), ('\xC8',"@`E"),
  ('\xCC',"@`I"), ('\xD2',"@`O"), ('\xD9',"@`U"),
  ('\xE2',"@^a"), ('\xEA',"@^e"),
  ('\xEE',"@^i"), ('\xF4',"@^o"),
  ('\xFB',"@^u"), ('\xC2',"@^A"),
  ('\xCA',"@^E"), ('\xCE',"@^I"),
  ('\xD4',"@^O"), ('\xDB',"@^U"),
  ('\xE5',"@oa"), ('\xC5',"@oA"),
  ('\xF8',"@/o"), ('\xD8',"@/O"),
  ('\xE3',"@~a"), ('\xF1',"@~n"), ('\xF5',"@~o"),
  ('\xC3',"@~A"), ('\xD1',"@~N"), ('\xD5',"@~O"),
  ('\xE6',"@ae"), ('\xC6',"@AE"),
  ('\xE7',"@cc"), ('\xC7',"@cC"),
  ('\xFE',"@th"), ('\xF0',"@et"), ('\xDE',"@Th"), ('\xD0',"@Et"),
  ('\xA3',"@LL"),
  ('\x0153',"@oe"), ('\x0152',"@OE"),
  ('\xA1',"@!!"), ('\xBF',"@??")]


{---------------}


ppVerbs :: [([String],[GrammarLine])] -> [String]

ppVerbs = concatMap ppVerb

ppVerb (verbs,grammars) =
  -- FIXME: check meta
  "" : unwords ("Verb" : map ppDictWord verbs)
     : map ppVerbGrammarLine grammars ++ [";"]

ppVerbGrammarLine (GrammarLine tokens action reverse) =
  tab 7 ("    * " ++ unwords (map ppVerbToken tokens))
        ("-> " ++ nameAction action ++ if reverse then " reverse" else "")

-- FIXME
ppVerbToken (ElementaryToken s) = s
ppVerbToken (Preposition p) = ppDictWord p
ppVerbToken (Attribute n) = nameAttr n
ppVerbToken (ParseRoutine prefix n) =
  prefix ++ ppConst' (TypeRoutinePtr TypeUnknown []) n
ppVerbToken (InfocomObject x y)    =
  ppInfocomObject1 x ++ ppInfocomObject2 y
ppVerbToken (Alternatives tokens) =
  join "/" (map ppVerbToken tokens)


ppInfocomObject1 0    = "object"
ppInfocomObject1 attr = nameAttr attr

ppInfocomObject2 0     = ""
ppInfocomObject2 flags = '(' : join "," flagNames ++ ")"
  where flagNames = [x | (n,x) <- allFlagNames, testBit flags n]
        allFlagNames =
         [(7,"held"),
          (6,"carried"),
          (5,"in_room"),
          (4,"on_ground"),
          (3,"take"),
          (2,"many"),
          (1,"have"),
          (0,"flagbit0")]


ppDictWord w = '\'' : ppString' w ++ "'"


{---------------}


ppGlobal n
  | sfGlobalSilent n  = []
  | val == 0 && type_ /= TypeAttr False
                      = [prefix ++ ";"]
  | otherwise         = [prefix ++ " = " ++ ppConst' type_ val ++ ";"]
  where prefix = "Global " ++ nameGlobal n
        val    = wordAt (hdrGlobalVarTableAddr + n * 2)
        type_  = sfGlobalType n


ppDictEntry (word,_,(DictMaybeTruncated:tags)) =
  ppDictEntry' word tags

ppDictEntry (word,_,tags) = ppDictEntry' word tags

ppDictEntry' word tags =
  tab 2 ("! " ++ ppDictWord word) (show tags)


{---------------}


ppArray (SFArray addr name info)
  | [line] <- body =
      [prefix ++ ' ' : unwords body ++ ';' : comment]
  | otherwise =
      (prefix ++ comment) : indentBlock body ++ [";"]
  where
    prefix     = "Array " ++ name ++ ' ' : typ
    comment    = "\t! " ++ showDecHex addr
    (body,typ) = getArrayBody addr info False


getArrayBody addr info@(ArrayInfo len init loop) forbidTable
  | isNothing realLength = (["! Unknown length -- cannot print"],typ)
  | otherwise = (body,typ)

  where

    isWords = all (== 2) (map fst (init ++ loop))

    typ = case (isWords,isTable) of
            (True, True ) -> "table"
            (False,True ) -> "string"
            (True, False) -> "-->"
            (False,False) -> "->"

    body :: [String]
    body
      | isEmpty   = [show ((if isWords then length else sum . map fst) (concat tableVals))]
      | isString  = [ppQuotedString (zCharsToUnicode (map val (concat tableVals)))]
      | otherwise = concatMap (maybeCombine . map ppArrayVal) tableVals

    maybeCombine :: [[String]] -> [String]
    maybeCombine ([x] : xs@([_] : _)) =
      (x ++ ' ' : y) : ys where (y:ys) = maybeCombine xs
    maybeCombine (x : xs) =
      x ++ maybeCombine xs
    maybeCombine [] = []

    val (_,Const _ _ v) = v

    realLength =
      case len of
        ConstLength n -> Just n
        UnknownLength -> Nothing
        StoredLength ->
          if (fst (head init) == 1 || (isWords && fst (head init) == 2))
            then Just (val (head (head vals)))
            else Nothing

    (isTable,tableVals)
      | StoredLength <- len, [_] <- init, [_] <- loop, not forbidTable
                    = (True,tail (head vals) : tail vals)
      | otherwise   = (False,vals)

    isString = all (char 1) (concat tableVals) ||
               all (char 2) (concat tableVals)

    char b (b',Const _ TypeZsciiChar n) = b == b' && n /= 0
    char b _  = False

    isEmpty = all zero (concat tableVals)

    zero (_,Const _ _ 0) = True
    zero _               = False

    vals =
      evalFrom addr $
        liftM2 (:) (mapM getVal init)
                   (liftM (take (fromMaybe 0 realLength))
                          (repeatUntilEmpty (mapM getVal loop)))

    getVal (b,t) =
      do pos <- getPos
         val <- if b==1 then getUByte else getUWord
         return (b,Const pos t val)

    ppArrayVal (2,k@(Const _ (TypeArrayPtr info) addr))
      | isWords && isNothing (nameArray addr) =
          ("[! " ++ showDecHex addr)
           : indentBlock (fst (getArrayBody addr info True))
           ++ ["]"]

    ppArrayVal (b,k@(Const _ t val)) =
      if b == (if isWords then 2 else 1) then
        [ppConst k]
      else
        case ppConst k of
          "0" -> ["0 0"]
          x -> ['(' : x ++ "/256) (" ++ x ++ "%256)"]


{---------------}


outputSymFile =
  header ++ warning
  ++ blankLine outputSymFileMD5
  ++ blankLine outputSymFileObjects
  ++ blankLine outputSymFileAttributes
  ++ blankLine outputSymFileProperties
  ++ blankLine outputSymFileGlobals
  ++ blankLine outputSymFileActions
  ++ blankLine outputSymFileWords
  ++ blankLine outputSymFileRoutines
  where
    header = ["! Auto-generated by Reform."]
    warning | null syms = []
            | otherwise = ["! WARNING: Contents of input symbol file are not reproduced here!"]
    blankLine [] = []
    blankLine xs = "" : xs

{-
copyInputSymFileDirectives =
  case concatMap reproduceSymFileDirective syms of
    [] -> []
    xs -> "" : "! retained from input info file" : xs

-- proper showHex behavior requires GHC 6.2

reproduceSymFileDirective (SFCodeArea (from,to)) =
  ["CodeArea\t0x" ++ showHex from ("\t0x" ++ showHex to "")]

reproduceSymFileDirective (SFStringArea (from,to)) =
  ["StringArea\t0x" ++ showHex from ("\t0x" ++ showHex to "")]

reproduceSymFileDirective (SFRoutineAt addr) =
  ["RoutineAt 0x" ++ showHex addr ""]

reproduceSymFileDirective _ = []
-}

outputSymFileMD5 =
  ["MD5\t" ++ storyFileMD5]

outputSymFileObjects =
  mapMaybe helper [1..numObjects] where
  helper n | sfObjectNamed n = Nothing
           | otherwise = Just ("Object " ++ show n ++ '\t' : nameObject n)

outputSymFileAttributes =
  mapMaybe helper allUsedAttribs
  where
    helper (n,users)
      | sfAttrNamed n = Nothing
      | otherwise = Just ("Attribute " ++ show n ++ '\t' : tab 3 (nameAttr n)
                           ("! " ++ join " " (map nameObject users)))
    allUsedAttribs =
      group $ sortFst $
        [(attr,obj) | attr <- [0..47], not (sfAttrNamed attr),
                      obj <- range (bounds objects),
                      testBit (snd3 (objects!obj)) (47-attr)]

snd3 (_,b,_) = b

outputSymFileProperties =
  mapMaybe helper allUsedProps
  where
    helper n | sfPropNamed n = Nothing
             | otherwise = Just $
                 "Property " ++ show n ++ '\t'
                   : nameProp n ++ ':' : ppSymFilePropType (propType n)
    allUsedProps =
      uniq $ sort $ concat [map fst ps | (_,_,ps) <- elems objects]

outputSymFileRoutines =
  concatMap helper routines
  where
    helper (addr,(params,_))
     | sfRoutineNamed addr = []
     | otherwise =
      let names = nameRoutine addr : map (nameLocal addr) [1..length params]
          types = sfRoutineType addr : sfLocalTypes addr
      in ppRoutineRefs addr []
         ++ ["Routine 0x" ++ showHex addr ('\t' :
              join " " [s ++ ':' : ppSymFileType t | s <- names | t <- types])]

{-
          falseEnd  = case sfFalseEnd addr of
                        0 -> []
                        n -> ["FalseEnd" ++ hexAddr ++ ' ' : show n]
          printRout = case sfPrintFunc (unpackRoutineAddr addr) of
                        Just name -> ["PrintRoutine" ++ hexAddr ++ '\t' : name]
                        Nothing   -> []
-}

outputSymFileGlobals =
  mapMaybe helper [0..lastGlobal]
  where
    helper n | sfGlobalNamed n = Nothing
             | otherwise = Just $
                 "Global " ++ show n ++ '\t' :
                   nameGlobal n ++ ':' : ppSymFileType (sfGlobalType n)

outputSymFileActions =
  mapMaybe helper allUsedActions
  where
    helper (act,uses)
      | sfActionNamed act = Nothing
      | otherwise = Just $
          "Action " ++ show act ++ '\t' : tab 2 (nameAction act)
             ("! " ++ join "\n\t\t\t\t! " (map helper2 uses))
    helper2 (verbWords,tokens) =
      unwords (map ppVerbToken (Alternatives (map Preposition verbWords) : tokens))
    allUsedActions =
      group $ sortFst
        [(act,(verbWords,tokens))
         | (verbWords,lines) <- verbs, GrammarLine tokens act _ <- lines]

outputSymFileWords =
  mapMaybe helper possiblyTruncatedWords
  where
    helper word | sfWordNamed word = Nothing
                | otherwise = Just ("Word\t" ++ word ++ '\t' : word)
    possiblyTruncatedWords = [word | (_,word,tags) <- elems dictionary,
                                DictMaybeTruncated `elem` tags]


ppSymFileType t =
  case t `lookup` [(b,a) | (a,b) <- wordTypeNames] of
    Just name -> name
    Nothing   ->
      case t of
        TypeArrayPtr info -> '^' : ppSymFileArrayType info
        TypeRoutinePtr t ts -> '^' : ppSymFileRoutineType (t:ts)
        _ -> "no_external_type_name"

ppSymFilePropType t =
  case t `lookup` [(b,a) | (a,b) <- propTypeNames] of
    Just name -> name
    Nothing   ->
      case t of
        PropTypeSingle t -> ppSymFileType t
        PropTypeMulti t  -> ppSymFileArrayType t

ppSymFileArrayType t@(ArrayInfo len init loop) =
  case t `lookup` [(b,a) | (a,b) <- arrayTypeNames] of
    Just name -> name
    Nothing -> "array(" ++ join "," (map foo init ++ last) ++ ")"
  where
    last | null loop = []
         | otherwise = [mult ++ '(' : join "," (map foo loop) ++ ")"]
    mult = case len of
             ConstLength k -> show k ++ "*"
             StoredLength  -> "n*"
             UnknownLength -> "*"
    foo (1,t) = '~' : ppSymFileType t
    foo (2,t) = ppSymFileType t

ppSymFileRoutineType ts =
  "routine(" ++ join "," (map ppSymFileType ts) ++ ")"


{---------------}


parenIf True x  = '(' : x ++ ")"
parenIf False x = x


ppJump 0 = "rfalse"
ppJump 1 = "rtrue"
ppJump n = "jump " ++ ppLabel n

ppLabel 0     = "rfalse"
ppLabel label = "label" ++ show label


join sep [] = []
join sep xs = foldr1 (\a b -> a ++ sep ++ b) xs


indentBlock = map indentLine

indentLine ('<' : x) = ' ':' ':x
indentLine x = ' ':' ':' ':' ':x


showDecAndHex n =
  "\t! " ++ show n ++ " / 0x" ++ showHex n ""


tab :: Int -> String -> String -> String
tab n x y =
  x ++ makeTabs (n - length x `div` 8) ++ y

makeTabs n | n <= 0    = " "
           | otherwise = replicate n '\t'
