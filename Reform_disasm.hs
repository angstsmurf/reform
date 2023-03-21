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


module Reform_disasm (
	disasmRoutine, routineMayFollow
) where


import Reform_storyfile
import Reform_zscii
import Reform_zcode
import Reform_symfile

import Data.Array
import Data.Bits
import Control.Monad


{------------}


routineMayFollow :: StreamReader Bool
routineMayFollow =
  do eos <- isEOS
     if eos then return False else do
     hdr <- peekUByte
     return (hdr <= 15)


--                             addr params body
disasmRoutine :: StreamReader (Int,([Int],[ZInstr]))
disasmRoutine =
  do addr <- getPos
     hdr  <- getRoutineHeader
     body <- disasm addr
     return (addr,(hdr,body))


{------------}


getRoutineHeader :: StreamReader [Int]		-- returns default vals of locals

getRoutineHeader = do
  numLocals <- getUByte
  if numLocals > 15
    then error "Too many locals"
    else replicateM numLocals getLocalDefaultValue

getLocalDefaultValue
  | ver [1..4]  = getUWord
  | otherwise   = return 0


{------------}


disasm addr = disasm' 0 (sfFalseEnd addr)

disasm' maxLabel extraLives
  | extraLives < 0  = return []
  | otherwise =
  do eos   <- isEOS
     if eos then return [] else do
     pos   <- getPos
     instr <- decodeInstr
     let (target,stop) =
           case instr of
             ZInstr _ op _ _ _ Nothing           -> (0,op `elem` stopOps)
             ZInstr _ op _ _ _ (Just (_,target)) -> (target,op == OJumpIf PrimTrue)
             ZReturn _   -> (0,True)
             ZEval _     -> (0,False)
         maxLabel' = max maxLabel target
         extraLives' =
           if maxLabel' <= pos && stop then
             extraLives - 1
           else
             extraLives
     rest <- disasm' maxLabel' extraLives'
     return (ZLabel pos Multi : instr : rest)

  where stopOps = [OStop,OReturn,OJumpIf PrimTrue]


decodeInstr = getUByte >>= decodeInstr1 >>= cvtJump


cvtJump (ZInstr j (OJumpIf PrimTrue) _ [Const _ _ ofs] Nothing Nothing) =
  do pos <- getPos
     let target | ofs == 0 || ofs == 1  = ofs
                | otherwise  = pos + (if ofs < 32768 then ofs else ofs-65536) - 2
     return (ZInstr j (OJumpIf PrimTrue) False [] Nothing (Just (True,target)))

cvtJump x = return x


decodeInstr1 x
  | x < 0x80  = decodeInstrLong x
  | x < 0xB0  = decodeInstrShort x
  | hdrVersion >= 5 && x == 0xBE  = decodeInstrExtended
  | x < 0xC0  = decodeInstrShort0 x
  | otherwise = decodeInstrVariable x

decodeInstrLong x =
  decodeInstr2 op [arg1,arg2] where
    op   = x .&. 0x1F
    arg1 = if testBit x 6 then 2 else 1
    arg2 = if testBit x 5 then 2 else 1

decodeInstrShort x =
  decodeInstr2 op [arg] where
    op  = x .&. 0xCF
    arg = (x `shiftR` 4) .&. 3

decodeInstrShort0 x =
  decodeInstr2 x []

decodeInstrVariable x = do
  types1 <- getUByte
  types2 <- if x `elem` disasmVarLongOps
             then getUByte
             else return 0xFF
  let op    = if x < 0xE0 then x .&. 0x1F else x
      args  = takeWhile (/= 3) (reverse (take 8 (helper types)))
      types = types1 * 256 + types2
      helper n = (n .&. 3) : helper (n `shiftR` 2)
  decodeInstr2 op args


disasmVarLongOps = [236,250]


decodeInstrExtended = do
  op <- getUByte
  decodeInstrVariable (op + 256)

decodeInstr2 :: Int -> [Int] -> StreamReader ZInstr

decodeInstr2 opcode argTypes = do
  args <- mapM decodeInstrArg argTypes
  (lookupOp opcode) args


decodeInstrArg 0 =
  do pos <- getPos
     arg <- getUWord
     return (Const pos TypeUnknown arg)

decodeInstrArg 1 =
  do pos <- getPos
     arg <- getUByte
     return (Const pos TypeUnknown arg)

decodeInstrArg 2 = liftM makeVar getUByte


plain n o args =
  return (ZInstr n o False args Nothing Nothing)

store n o args =
  do store <- getUByte
     return (ZInstr n o False args (Just (makeVar store)) Nothing)

branch n o args =
  do branch <- getBranch
     return (ZInstr n o False args Nothing branch)

storeBranch n o args =
  do store <- getUByte
     branch <- getBranch
     return (ZInstr n o False args (Just (makeVar store)) branch)

indir f n o args =
  do (ZInstr a b False c d e) <- f n o args
     return (ZInstr a b True c d e)

ret val [] =
  return (ZInstr "ret" OReturn False [val] Nothing Nothing)

printInline ret [] = do
  pos <- getPos
  skipZString
  return $ (if ret then ZReturn else ZEval)
           (Call (Prim PrimPrintInline) [Const pos TypeStringAddr pos])

skipZString = do
  x <- getUWord
  when (x < 32768) skipZString


{-
typeArgs [] xs = xs
typeArgs (t:ts) [] = []
typeArgs (t:ts) (x:xs) = typeArg t x : typeArgs ts xs


typeArg t@(TypeArrayPtr _) (Const a _ n) =
  case sfArrayType n of
    Just t' -> Const a t' n
    Nothing -> Const a t n

typeArg t@(TypeRoutinePtr _ _) (Const a _ n) =
  Const a (TypeRoutinePtr (sfRoutineType n') (sfLocalTypes n')) n
  where n' = unpackRoutineAddr n

typeArg t (Const a _ n) = Const a t n
typeArg t x             = x
-}

getBranch = do
  x <- getUByte
  let dir     = testBit x 7
      oneByte = testBit x 6
      ofs1    = x .&. 63
  ofs <- if oneByte
           then return ofs1
           else do ofs2 <- getUByte
                   let ofs' = (ofs1 * 256 + ofs2)
                   return (if ofs' < 8192 then ofs' else ofs' - 16384)
  pos <- getPos
  if ofs == 0 || ofs == 1 then
    return (Just (dir, ofs))
   else if ofs == 2 then
    return Nothing
   else
    return (Just (dir, pos+ofs-2))


lookupOp opcode
  | opcode <= snd (bounds opcodeInfo) = opcodeInfo ! opcode
  | otherwise = (plain ("EXT:" ++ show (opcode-256)) $ OSpecial [])


opcodeInfo :: Array Int ([Expr] -> StreamReader ZInstr)

opcodeInfo =
  array (0, 255 + length opcodeInfoExt) $
    zip [0..] opcodeInfo0 ++
    zip [128..] opcodeInfo128 ++
    zip [176..] opcodeInfo176 ++
    zip [224..] opcodeInfo224 ++
    zip [256..] opcodeInfoExt

opcodeInfo0 =
 [plain		"2OP:0"		$ OSpecial [],
  branch	"je"		$ OJumpIf PrimEq,
  branch	"jl"		$ OJumpIf PrimLt,
  branch	"jg"		$ OJumpIf PrimGt,
  indir branch	"dec_chk"	$ OJumpIf PrimXDecChk,
  indir branch	"inc_chk"	$ OJumpIf PrimXIncChk,
  branch	"jin"		$ OJumpIf PrimIn,
  branch	"test"		$ OJumpIf PrimBitTest,
  store		"or"		$ OPrim PrimBitOr,
  store		"and"		$ OPrim PrimBitAnd,
  branch	"test_attr"	$ OJumpIf PrimHas,
  plain		"set_attr"	$ OPrim PrimFSet,
  plain		"clear_attr"	$ OPrim PrimFClear,
  indir plain	"store"		$ OStore,
  plain		"insert_obj"	$ OPrim PrimMove,
  store		"loadw"		$ OPrim PrimLoadW,
  store		"loadb"		$ OPrim PrimLoadB,
  store		"get_prop"	$ OPrim PrimGetProp,
  store		"get_prop_addr"	$ OPrim PrimGetPropAddr,
  store		"get_next_prop"	$ OPrim PrimGetNextProp,
  store		"add"		$ OPrim PrimPlus,
  store		"sub"		$ OPrim PrimMinus,
  store		"mul"		$ OPrim PrimTimes,
  store		"div"		$ OPrim PrimDivide,
  store		"mod"		$ OPrim PrimMod,
  store		"call_2s"	$ OPrim PrimInvoke,
  plain		"call_2n"	$ OPrim PrimInvoke,
  plain		"set_colour"	$ OSpecial ints,
  plain		"throw"		$ OStop,
  plain		"2OP:29"	$ OSpecial [],
  plain		"2OP:30"	$ OSpecial [],
  plain		"2OP:31"	$ OSpecial []]

opcodeInfo128 =
 [branch	"jz"		$ OJumpIf PrimEq0,
  storeBranch	"get_sibling"	$ OPrim PrimGetSibling,
  storeBranch	"get_child"	$ OPrim PrimGetChild,
  store		"get_parent"	$ OPrim PrimGetParent,
  store		"get_prop_len"	$ OPrim PrimGetPropLen,
  indir plain	"inc"		$ OPrim PrimXPreInc,
  indir plain	"dec"		$ OPrim PrimXPreDec,
  plain		"print_addr"	$ OPrim PrimPrintAddr,
  store		"call_1s"	$ OPrim PrimInvoke,
  plain		"remove_obj"	$ OPrim PrimRemove,
  plain		"print_obj"	$ OPrim PrimPrintObj,
  plain		"ret"		$ OReturn,
  plain		"jump"		$ OJumpIf PrimTrue,
  plain		"print_paddr"	$ OPrim PrimPrintPaddr,
  indir store	"load"		$ OLoad,
 if hdrVersion < 5 then
  store		"not"		$ OPrim PrimBitNot
 else
  plain		"call_1n"	$ OPrim PrimInvoke]

opcodeInfo176 =
 [ret (Const 0 TypeBool 1),		-- rtrue
  ret (Const 0 TypeBool 0),		-- rfalse
  printInline False,			-- print
  printInline True,			-- print_ret
  plain		"nop"		$ ONop,
  (if hdrVersion < 4 then branch else store) "save"	$ OSpecial [],
  (if hdrVersion < 4 then branch else store) "restore"	$ OSpecial [],
  plain		"restart"	$ OSpecial [],
  ret SP,				-- ret_popped
-- Note: Annoyotron is a V5 game, yet uses @pop several times. This breaks TXD also.
 if hdrVersion < 5 then
  plain		"pop"		$ OSpecial []
 else
  store		"catch"		$ OSpecial [],
  plain		"quit"		$ OStop,
  plain		"new_line"	$ OPrim PrimNewline,
  plain		"show_status"	$ OSpecial [],
  branch	"verify"	$ OSpecial [],
  plain		"[EXT]"		$ OSpecial [],
  branch	"piracy"	$ OSpecial []]

opcodeInfo224 =
 [if hdrVersion < 4 then
  store		"call"		$ OPrim PrimInvoke
 else
  store		"call_vs"	$ OPrim PrimInvoke,
  plain		"storew"	$ OPrim PrimStoreW,
  plain		"storeb"	$ OPrim PrimStoreB,
  plain		"put_prop"	$ OPrim PrimPutProp,
 if hdrVersion < 5 then
  plain		"sread"		$ OSpecial readArgs
 else
  store		"aread"		$ OSpecial readArgs,
  plain		"print_char"	$ OPrim PrimPrintChar,
  plain		"print_num"	$ OPrim PrimPrintNum,
  store		"random"	$ OPrim PrimRandom,
  plain		"push"		$ OPush,
  (if hdrVersion == 6 then store else indir plain) "pull"	$ OPull,
  plain		"split_window"	$ OSpecial int,
  plain		"set_window"	$ OSpecial int,
  store		"call_vs2"	$ OPrim PrimInvoke,
  plain		"erase_window"	$ OSpecial int,
  plain		"erase_line"	$ OSpecial int,
  plain		"set_cursor"	$ OSpecial ints,
  plain		"get_cursor"	$ OSpecial [anonArray],
  plain		"set_text_style"$ OPrim PrimSetTextStyle,
  plain		"buffer_mode"	$ OSpecial [TypeBool],
  plain		"output_stream"	$ OSpecial [TypeInt, anonArray, TypeInt],
  plain		"input_stream"	$ OSpecial int,
  plain		"sound_effect"	$ OSpecial [TypeInt, TypeInt, TypeInt, anonRoutine],
  store		"read_char"	$ OSpecial [TypeInt, TypeInt, anonRoutine],
  storeBranch	"scan_table"	$ OSpecial [TypeUnknown, anonArray, TypeInt, TypeInt],
  store		"not"		$ OPrim PrimBitNot,
  plain		"call_vn"	$ OPrim PrimInvoke,
  plain		"call_vn2"	$ OPrim PrimInvoke,
  plain		"tokenise"	$ OSpecial [anonArray, anonArray, TypeUnknown, TypeBool],
  plain		"encode_text"	$ OSpecial [anonArray, TypeInt, TypeInt, anonArray],
  plain		"copy_table"	$ OSpecial [anonArray, anonArray, TypeInt],
  plain		"print_table"	$ OSpecial [anonArray, TypeInt, TypeInt, TypeInt],
  branch	"check_arg_count"$ OSpecial int]

opcodeInfoExt =
 [store		"save"		$ OSpecial [anonArray, TypeInt, anonArray],
  store		"restore"	$ OSpecial [anonArray, TypeInt, anonArray],
  store		"log_shift"	$ OSpecial ints,
  store		"art_shift"	$ OSpecial ints,
  store		"set_font"	$ OSpecial int,
  plain		"draw_picture"	$ OSpecial ints,
  branch	"picture_data"	$ OSpecial [TypeInt, anonArray],
  plain		"erase_picture"	$ OSpecial ints,
  plain		"set_margins"	$ OSpecial ints,
  store		"save_undo"	$ OSpecial [],
  store		"restore_undo"	$ OSpecial [],
  plain		"print_unicode"	$ OSpecial [TypeUnicodeChar],
  store		"check_unicode"	$ OSpecial [TypeUnicodeChar],
  plain		"EXT:13"	$ OSpecial [],
  plain		"EXT:14"	$ OSpecial [],
  plain		"EXT:15"	$ OSpecial [],
  plain		"move_window"	$ OSpecial ints,
  plain		"window_size"	$ OSpecial ints,
  plain		"window_style"	$ OSpecial ints,
  store		"get_wind_prop"	$ OSpecial ints,
  plain		"scroll_window"	$ OSpecial ints,
  plain		"pop_stack"	$ OSpecial [TypeInt, TypeUnknown],
  plain		"read_mouse"	$ OSpecial [anonArray],
  plain		"mouse_window"	$ OSpecial int,
  branch	"push_stack"	$ OSpecial [TypeInt, TypeUnknown],
  plain		"put_wind_prop"	$ OSpecial ints,	-- FIXME: some properties are routines?
  plain		"print_form"	$ OSpecial [anonArray],
  branch	"make_menu"	$ OSpecial [TypeInt, anonArray],
  plain		"picture_table"	$ OSpecial [anonArray]]


readArgs  = [anonArray, anonArray, TypeInt, anonRoutine]
int       = [TypeInt]
ints      = [TypeInt,TypeInt,TypeInt,TypeInt]


anonArray = TypeArrayPtr (ArrayInfo UnknownLength [] [(1,TypeUnknown)])
anonRoutine = TypeRoutinePtr TypeUnknown []
