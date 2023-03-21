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


module Reform_zcode (
	ZInstr(..), LabelType(..), Expr(..), FuncType(..),
	ZType(..),
	ArrayInfo(..), ArrayLengthInfo(..),
	EnumInfo(..),
	ZOperation(..),
	Prim(..), PrimJump(..),
	makeVar
) where

import Data.Ix


--                    name     op       indir  args     store          branch
data ZInstr = ZInstr String !ZOperation !Bool [Expr] (Maybe Expr) (Maybe (Bool,Int))
            | ZPush Expr | ZEval Expr | ZReturn Expr
            | ZJump Int | ZJCond Expr Int
            | ZLabel Int LabelType | ZExitLabels [Int]
            | ZIfThenElse Expr [ZInstr] [ZInstr]
            | ZSpecial String | ZNop
  deriving (Eq)


data LabelType = Single | Multi  deriving (Eq)


data Expr = SP | Local Int | Global Int
          | Const Int ZType Int {- addr, type, val -}
          | Assignment Expr Expr | IfThenExpr Expr [Expr] [Expr]
          | Call FuncType [Expr]
  deriving (Eq)


data ZType = TypeUnknown
           | TypeObject | TypeProp | TypeAttr Bool
           | TypeRoutinePtr ZType [ZType]
           | TypeStringAddr | TypeStringPaddr
           | TypeZsciiChar | TypeUnicodeChar
           | TypeInt | TypeBool | TypeEnum EnumInfo
           | TypeAction | TypeDictWord
           | TypeAdjectiveNum | TypeVerbNum
	   | TypeArrayPtr ArrayInfo
           | TypeThing
  deriving (Show,Eq)

data ArrayInfo =
  ArrayInfo ArrayLengthInfo [(Width,ZType)] [(Width,ZType)]
--          word-array        pre-cycle        cycle
  deriving (Show,Eq)

type Width = Int

data ArrayLengthInfo = StoredLength | ConstLength Int | UnknownLength
  deriving (Show,Eq)

data EnumInfo = EnumInfo ![(Int,String)] !String
  deriving (Show,Eq)


data ZOperation
  = OSpecial [ZType]
  | OPrim !Prim
  | OJumpIf !PrimJump
  | OReturn
  | OLoad | OStore
  | OPush | OPull
  | ONop
  | OStop
  deriving (Eq)


{-------------}


data FuncType = Prim !Prim | PrimJump !PrimJump !Bool
  deriving (Eq)

-- "PrimJump" contains primitives which branch; "Prim"
-- contains others. The few non-boolean primitives
-- which branch are in "Prim", and are handled by a
-- special case in the decompiler. The peculiar short-
-- circuiting && and || operators are in "Prim" for
-- convenience.
--
-- Primitives with an "X" in them are those which cannot
-- be implemented as functions because they modify their
-- arguments.

data Prim
  = PrimInvoke
  | PrimAnd | PrimOr
  | PrimGetProp | PrimGetPropAddr | PrimGetPropLen | PrimPutProp
  | PrimPlus | PrimMinus | PrimTimes | PrimDivide | PrimMod
  | PrimBitOr | PrimBitAnd | PrimBitNot
  | PrimLogNot
  | PrimLoadB | PrimLoadW
  | PrimStoreB | PrimStoreW
  | PrimGetParent | PrimGetChild | PrimGetSibling
  | PrimMove | PrimRemove
  | PrimGetNextProp
  | PrimFSet | PrimFClear
  | PrimRandom | PrimSetTextStyle
  | PrimXPreInc | PrimXPostInc
  | PrimXPreDec | PrimXPostDec
  | PrimNewline | PrimPrintInline
  | PrimPrintAddr | PrimPrintPaddr | PrimPrintObj
  | PrimPrintChar | PrimPrintNum
  deriving (Bounded,Ix,Eq,Ord)


data PrimJump
  = PrimTrue
  | PrimIn | PrimHas
  | PrimEq | PrimLt | PrimGt | PrimEq0
  | PrimBitTest
  | PrimXDecChk | PrimXIncChk
  deriving (Bounded,Ix,Eq,Ord)


{-------------}


makeVar x
  | x == 0    = SP
  | x < 16    = Local x
  | otherwise = Global (x-16)
