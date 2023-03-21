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


module Reform_inference (
	inferStmtTypes
) where


import Reform_zcode
import Reform_storyfile
import Reform_symfile
import Reform_objects

import Data.Array
import Data.Maybe


type LookupLocal = Int -> ZType


inferStmtTypes :: ZType -> LookupLocal -> [ZInstr] -> [ZInstr]

inferStmtTypes rtn loc stmt =
  let ?returnType  = rtn
      ?lookupLocal = loc
  in inferStmtTypes' stmt


inferStmtTypes' s = map inferStmtTypes'' s


inferStmtTypes'' orig@(ZInstr name op indir args store branch) =
  ZInstr name op indir
         (zipWith inferExprTypes (foo (types ++ repeat TypeUnknown)) args)
         store branch
  where types =
          case op of
            OSpecial argTypes -> argTypes
            OPrim p   -> (primArgTypes!p) ?lookupLocal TypeUnknown args
            OJumpIf p -> (primJumpArgTypes!p) ?lookupLocal args
            _         -> []
        foo xs
          | indir     = TypeInt : tail xs
          | otherwise = xs

inferStmtTypes'' (ZPush expr) =
  ZPush (inferExprTypes TypeUnknown expr)

inferStmtTypes'' (ZEval expr) =
  ZEval (inferExprTypes TypeUnknown expr)

inferStmtTypes'' (ZReturn expr) =
  ZReturn (inferExprTypes ?returnType expr)

inferStmtTypes'' (ZJCond expr dest) =
  ZJCond (inferExprTypes TypeBool expr) dest

inferStmtTypes'' (ZIfThenElse pred yes no) =
  ZIfThenElse (inferExprTypes TypeBool pred)
              (inferStmtTypes' yes)
              (inferStmtTypes' no)

inferStmtTypes'' x = x


inferExprTypes t expr
  | noConstants expr  = expr
  | otherwise = inferExprTypes' t expr


noConstants (Assignment left right) =
  noConstants left && noConstants right

noConstants (IfThenExpr x ys zs) =
  noConstants x && all noConstants ys && all noConstants zs

noConstants (Call _ xs) =
  all noConstants xs

noConstants (Const _ _ _) = False

noConstants _ = True


inferExprTypes' t (Const ad TypeUnknown val) =
  Const ad t val

inferExprTypes' t (Assignment left right) =
  Assignment (inferExprTypes' t' left) (inferExprTypes' t' right)
  where
    t' = case t of
           TypeUnknown -> findTypeList $ map (findType ?lookupLocal FindGeneral) [left,right]
           _           -> t

inferExprTypes' t (IfThenExpr cond left right) =
  IfThenExpr (inferExprTypes' TypeBool cond) (f left) (f right)
  where f list = map (inferExprTypes' TypeUnknown) (init list)
                  ++ [inferExprTypes' t (last list)]

inferExprTypes' t (Call pr@(Prim p) args) =
  Call pr $ zipWith inferExprTypes'
                    ((primArgTypes!p) ?lookupLocal t args ++ repeat TypeUnknown)
                    args

inferExprTypes' t (Call pr@(PrimJump p _) args) =
  Call pr $ zipWith inferExprTypes'
                    ((primJumpArgTypes!p) ?lookupLocal args)
                    args

inferExprTypes' t x = x


data FindType = FindGeneral | FindRoutine | FindArray


findType :: LookupLocal -> FindType -> Expr -> ZType

findType loc t (Local n)          = loc n
findType loc t (Global n)         = sfGlobalType n
findType loc t (Const _ TypeUnknown val) = findType' t val
findType loc _ (Const _ t _)      = t
findType loc t (Assignment l r)   = findTypeList [findType loc t l, findType loc t r]
findType loc t (IfThenExpr _ x y) = findTypeList [findType loc t (last x), findType loc t (last y)]
findType loc t (Call (Prim p) args)   = (primReturnTypes!p) loc args
findType loc t (Call (PrimJump _ _) args) = TypeBool

findType _ _ _ = TypeUnknown


findType' FindGeneral _ = TypeUnknown

findType' FindRoutine addr =
  TypeRoutinePtr (sfRoutineType addr') (sfLocalTypes addr')
  where addr' = unpackRoutineAddr addr

findType' FindArray addr =
  fromMaybe TypeUnknown (sfArrayType addr)


findTypeList [x] = x
findTypeList (TypeUnknown:xs) = findTypeList xs
findTypeList (x:xs) = x


primReturnTypes :: Array Prim (LookupLocal -> [Expr] -> ZType)
primReturnTypes =
  accumArray (\a b -> b) (\_ _ -> TypeUnknown) (minBound,maxBound)
  [(PrimInvoke,		\loc (func:_) -> case findType loc FindRoutine func of
                                           TypeRoutinePtr rtn _ -> rtn
                                           _                    -> TypeUnknown),
   (PrimAnd,		\_ _ -> TypeBool),
   (PrimOr,		\_ _ -> TypeBool),
   (PrimGetProp,	\loc [_,prop] -> inferPropType prop),
   (PrimGetPropAddr,	\loc [_,prop] -> inferPropAddrType prop),
   (PrimGetPropLen,	\_ _ -> TypeInt),
-- FIXME: +, - unsafe!
   (PrimPlus,		\_ _ -> TypeInt),		-- FIXME: unsafe
   (PrimMinus,		\_ _ -> TypeInt),		-- FIXME: unsafe
   (PrimTimes,		\_ _ -> TypeInt),
   (PrimDivide,		\_ _ -> TypeInt),
   (PrimMod,		\_ _ -> TypeInt),
   (PrimBitOr,		\_ _ -> TypeInt),
   (PrimBitAnd,		\_ _ -> TypeInt),
   (PrimBitNot,		\_ _ -> TypeInt),
   (PrimLogNot,		\_ _ -> TypeBool),
   (PrimLoadB,		\loc [arg1,arg2] -> inferElemType 1 loc arg1 arg2),
   (PrimLoadW,		\loc [arg1,arg2] -> inferElemType 2 loc arg1 arg2),
   (PrimGetParent,	\_ _ -> TypeObject),
   (PrimGetChild,	\_ _ -> TypeObject),
   (PrimGetSibling,	\_ _ -> TypeObject),
   (PrimGetNextProp,	\_ _ -> TypeProp),
   (PrimRandom,		\_ _ -> TypeInt),
   (PrimXPreInc,	\loc [arg] -> findType loc FindGeneral arg),
   (PrimXPostInc,	\loc [arg] -> findType loc FindGeneral arg),
   (PrimXPreDec,	\loc [arg] -> findType loc FindGeneral arg),
   (PrimXPostDec,	\loc [arg] -> findType loc FindGeneral arg)]


inferElemType width loc arg1 arg2 =
  case findType loc FindArray arg1 of
    TypeArrayPtr (ArrayInfo _ init loop)
      -> inferElemType' width init loop arg2
    _ -> TypeUnknown

inferElemType' width init loop (Const _ _ n)
  | n' < sum (map fst init)  = helper n' init
  | otherwise  =
      helper ((n' - sum (map fst init)) `mod` sum (map fst loop)) loop
  where
    n' = n * width
    helper (-1) _ = TypeUnknown
    helper 0 ((w,t):_) | w == width  = t
    helper 0 _ = TypeUnknown
    helper n ((w,t):rest) = helper (n-w) rest

inferElemType' width init loop@(elem@(w,t):_) _
  | w == width && all (elem==) loop  = t
  | otherwise  = TypeUnknown


primArgTypes :: Array Prim (LookupLocal -> ZType -> [Expr] -> [ZType])
primArgTypes =
  array (minBound,maxBound)
  [(PrimInvoke,		\loc _ (func:_) -> case findType loc FindRoutine func of
                                             t@(TypeRoutinePtr _ ts) -> (t:ts)
                                             _ -> [TypeRoutinePtr TypeUnknown []]),
   (PrimAnd,		\_ _ _ -> [TypeBool,TypeBool]),
   (PrimOr,		\_ _ _ -> [TypeBool,TypeBool]),
   (PrimGetProp,	\_ _ _ -> [TypeObject,TypeProp]),
   (PrimGetPropAddr,	\_ _ _ -> [TypeObject,TypeProp]),
   (PrimGetPropLen,	\_ _ _ -> [TypeUnknown]),
   (PrimPutProp,	\_ _ [_,p,_] -> [TypeObject,TypeProp,inferPropType p]),
-- FIXME: +, - unsafe!
   (PrimPlus,		\_ _ _ -> [TypeInt,TypeInt]),			-- arg2 int if arg1 not int, and vice versa
   (PrimMinus,		\_ _ _ -> [TypeInt,TypeInt]),			-- arg1 and return have same type, in some cases
   (PrimTimes,		\_ _ _ -> [TypeInt,TypeInt]),
   (PrimDivide,		\_ _ _ -> [TypeInt,TypeInt]),
   (PrimMod,		\_ _ _ -> [TypeInt,TypeInt]),
   (PrimBitOr,		\_ _ _ -> [TypeInt,TypeInt]),
   (PrimBitAnd,		\_ _ _ -> [TypeInt,TypeInt]),
   (PrimBitNot,		\_ _ _ -> [TypeInt]),
   (PrimLogNot,		\_ _ _ -> [TypeBool]),	-- FIXME: safe?
   (PrimLoadB,		\_ _ _ -> [anonArray,TypeInt]),
   (PrimLoadW,		\_ _ _ -> [anonArray,TypeInt]),
   (PrimStoreB,		\loc _ [arg1,arg2,_] -> [anonArray,TypeInt,inferElemType 1 loc arg1 arg2]),
   (PrimStoreW,		\loc _ [arg1,arg2,_] -> [anonArray,TypeInt,inferElemType 2 loc arg1 arg2]),
   (PrimGetParent,	\_ _ _ -> [TypeObject]),
   (PrimGetChild,	\_ _ _ -> [TypeObject]),
   (PrimGetSibling,	\_ _ _ -> [TypeObject]),
   (PrimMove,		\_ _ _ -> [TypeObject,TypeObject]),
   (PrimRemove,		\_ _ _ -> [TypeObject]),
   (PrimGetNextProp,	\_ _ _ -> [TypeObject,TypeProp]),
   (PrimFSet,		\_ _ _ -> [TypeObject,TypeAttr False]),
   (PrimFClear,		\_ _ _ -> [TypeObject,TypeAttr False]),
   (PrimRandom,		\_ _ _ -> [TypeInt]),
   (PrimSetTextStyle,	\_ _ _ -> [TypeInt]),
   (PrimXPreInc,	\_ t _ -> [t]),
   (PrimXPostInc,	\_ t _ -> [t]),
   (PrimXPreDec,	\_ t _ -> [t]),
   (PrimXPostDec,	\_ t _ -> [t]),
   (PrimNewline,	\_ _ _ -> []),
   (PrimPrintInline,	\_ _ _ -> [TypeStringAddr]),
   (PrimPrintAddr,	\_ _ _ -> [TypeStringAddr]),
   (PrimPrintPaddr,	\_ _ _ -> [TypeStringPaddr]),
   (PrimPrintObj,	\_ _ _ -> [TypeObject]),
   (PrimPrintChar,	\_ _ _ -> [TypeZsciiChar]),
   (PrimPrintNum,	\_ _ _ -> [TypeInt])]

anonArray = TypeArrayPtr (ArrayInfo UnknownLength [] [(2,TypeUnknown)])


inferPropType (Const _ _ prop) =
  case propType prop of
    PropTypeSingle t -> t
    _                -> TypeUnknown

inferPropType _ = TypeUnknown


inferPropAddrType (Const _ _ prop) =
  case propType prop of
    PropTypeMulti t -> TypeArrayPtr t
    _               -> TypeUnknown

inferPropAddrType _ = TypeUnknown


primJumpArgTypes :: Array PrimJump (LookupLocal -> [Expr] -> [ZType])
primJumpArgTypes =
  array (minBound,maxBound)
  [(PrimIn,		\_ _ -> repeat TypeObject),
   (PrimHas,		\_ _ -> TypeObject : repeat (TypeAttr False)),
   (PrimEq,		equalTypes),
-- FIXME: would be better not to assume ints for < and >
   (PrimLt,		\_ _ -> repeat TypeInt),	-- was equalTypes
   (PrimGt,		\_ _ -> repeat TypeInt),	-- was equalTypes
   (PrimEq0,		\_ _ -> [TypeUnknown]),
   (PrimBitTest,	\_ _ -> [TypeInt,TypeInt]),
   (PrimXDecChk,	\_ _ -> [TypeInt,TypeInt]),	-- was equalTypes
   (PrimXIncChk,	\_ _ -> [TypeInt,TypeInt])]	-- was equalTypes


equalTypes loc args =
  repeat (findTypeList (map (findType loc FindGeneral) args))
