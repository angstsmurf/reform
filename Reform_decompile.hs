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


module Reform_decompile (
    decompile
) where


import Reform_zcode

import Data.List (sort)
import Data.Maybe (fromMaybe)


decompile        :: [ZInstr] -> [ZInstr]


decompile =
  doToDeath (decompileIf . doToDeath decompile')


doToDeath f code =
  let code' = f (updateLabels code) in
  if code == code' then code else doToDeath f code'


updateLabels code =
  updateLabels' (sort (findJumps code)) code

findJumps = concatMap findJumps'

findJumps' (ZInstr _ _ _ _ _ (Just (_,dst))) = [dst]
findJumps' (ZJCond _ dst)                    = [dst]
findJumps' (ZJump dst)                       = [dst]
findJumps' (ZIfThenElse _ yes no)            = findJumps yes ++ findJumps no
findJumps' _ = []

updateLabels' jumps (ZLabel addr _ : rest) =
  let (jumpsHere,jumpsLater) = break (> addr) (dropWhile (< addr) jumps)
      rest' = updateLabels' jumpsLater rest
  in updateLabel addr jumpsHere ++ rest'

updateLabels' jumps (ZIfThenElse cond yes no : rest) =
  ZIfThenElse cond (updateLabels' jumps yes) (updateLabels' jumps no) : updateLabels' jumps rest
-- FIXME: more control structures

updateLabels' jumps (x:xs) = x : updateLabels' jumps xs

updateLabels' jumps [] = []

updateLabel ad []  = []
updateLabel ad [_] = [ZLabel ad Single]
updateLabel ad _   = [ZLabel ad Multi]


decompile' (ZPush expr : ZInstr name op indir args store branch : rest) | hasSP args =
  decompile' (ZInstr name op indir (substSP expr args) store branch : rest)

decompile' (ZPush expr : ZInstr _ OPull True [Const _ _ var] Nothing Nothing : rest) | var /= 0  =
  decompile' (ZEval (Assignment (makeVar var) expr) : rest)

decompile' (ZPush expr : ZEval (Call (Prim PrimXPreInc) [expr']) : rest)
  | expr == expr' && noSideEffects expr
  = decompile' (ZPush (Call (Prim PrimXPostInc) [expr]) : rest)

decompile' (ZPush expr : ZEval (Call (Prim PrimXPreDec) [expr']) : rest)
  | expr == expr' && noSideEffects expr
  = decompile' (ZPush (Call (Prim PrimXPostDec) [expr]) : rest)

decompile' (ZJCond cond1 ad2 : ZJCond cond2 ad2' : rest) | ad2 == ad2'  =
  decompile' (ZJCond (mergeOrs True cond1 cond2) ad2 : rest)

decompile' (ZJCond cond1 ad2 : ZJCond cond2 ad3 : rest) | isAddr ad2 rest =
  decompile' (ZJCond (mergeOrs False (negateBoolExpr cond1) cond2) ad3 : rest)

decompile' (ZEval (Assignment (Global 239) (Call (Prim PrimLoadW) [Const _ _ 0, Const _ _ 8]))
          : ZEval (Assignment (Global 239) (Call (Prim p)         [Const _ _ x, Global 239]))
          : ZEval (Call (Prim PrimStoreW) [Const _ _ 0, Const _ _ 8, Global 239])
          : rest)
  | x == 65533 && p == PrimBitAnd  = decompile' (ZSpecial "font on" : rest)
  | x == 2     && p == PrimBitOr   = decompile' (ZSpecial "font off" : rest)

decompile' (ZJump ad : rest@(ZExitLabels ads:_)) | ad `elem` ads =
  decompile' rest


decompile' (i@(ZInstr _ _ _ args _ _) : rest)
  | ready args  = fromMaybe i (decompileReady i) : decompile' rest
  | otherwise   = i : decompile' rest


decompile' (ZIfThenElse cond yes no : rest) =
  ZIfThenElse cond (decompile' yes) (decompile' no) : decompile' rest

decompile' (x : xs) = x : decompile' xs

decompile' [] = []


isAddr l (ZLabel l' _ : _) = l == l'
isAddr l (ZExitLabels ls : _) = l `elem` ls
isAddr l _ = False


noSideEffects (Local _) = True
noSideEffects (Global _) = True
noSideEffects (Const _ _ _) = True
noSideEffects (IfThenExpr x y z) =
  noSideEffects x && all noSideEffects y && all noSideEffects z

-- FIXME: look at primitives in Call expressions?
noSideEffects _ = False


decompileIf (first@(ZJCond cond midLabel) : rest) =
  case findIfThenElse midLabel rest of
    Just (thenClause,elseClause,rest') ->
      makeIfThenElse (negateBoolExpr cond)
                     (decompileIf thenClause)
                     (decompileIf elseClause)
       : decompileIf rest'
    Nothing ->
      case findMatch (matchExitPoint midLabel) rest of
        Just (thenClause,(exit,rest')) ->
          ZIfThenElse (negateBoolExpr cond)
                      (decompileIf (thenClause [exit])) []
           : decompileIf rest'
        Nothing -> first : decompileIf rest

decompileIf (ZIfThenElse cond yes no : rest) =
  ZIfThenElse cond (decompileIf yes) (decompileIf no) : decompileIf rest

decompileIf (x : xs) = x : decompileIf xs

decompileIf [] = []


findIfThenElse midLabel rest =
  do (yes,(endLabel,rest)) <- findMatch (matchMiddle midLabel) rest
     (no,(exit,rest))      <- findMatch (matchExitPoint endLabel) rest
     return (yes [exit],no [exit],rest)

matchMiddle l (ZJump endLabel : ZLabel l' Single : rest) | l == l'  =
  Just (endLabel,rest)

matchMiddle l _ = Nothing

matchExitPoint label all@(ZLabel l _ : _) | label == l =
  Just (ZExitLabels [label],all)
matchExitPoint label all@[ZExitLabels ls] | label `elem` ls =
  Just (ZExitLabels (label:ls),all)
matchExitPoint label _ =
  Nothing


makeIfThenElse pred yes no
  | Just a <- helper yes,
    Just b <- helper no
      = ZPush (IfThenExpr pred a b)
  | otherwise
      = ZIfThenElse pred yes no
  where
    helper [ZPush x]                = Just [x]
    helper [ZPush x, ZExitLabels _] = Just [x]
    helper (ZEval x : rest)         = fmap (x:) (helper rest)
    helper (ZIfThenElse pred yes no : rest)
      | Just a <- helper yes,
        Just b <- helper no    = fmap (IfThenExpr pred a b :) (helper rest)
    helper _                   = Nothing


findMatch :: ([a] -> Maybe b) -> [a] -> Maybe ([a]->[a], b)

findMatch = findMatch' id

findMatch' sofar f [] = Nothing
findMatch' sofar f (x:xs) =
  case f (x:xs) of
    Just result -> Just (sofar,result)
    Nothing     -> findMatch' (sofar.(x:)) f xs


-- FIXME: correct?
mergeOrs or a@(Call (PrimJump p b) (x:y)) (Call (PrimJump p' b') (x':y'))
  | or == b && or == b' && p == p' && isMultiPrim p && noSideEffects x && x == x'
  = Call (PrimJump p b) (x : y ++ y')

mergeOrs or p q =
  Call (if or then Prim PrimOr else Prim PrimAnd) [p,q]

isMultiPrim PrimIn  = True
isMultiPrim PrimEq  = True
-- omit <, >, and has, since their behavior is so weird
isMultiPrim _ = False


negateExpr (Call (PrimJump x b) args) =
  Call (PrimJump x (not b)) args

negateExpr (Call (Prim PrimAnd) [p,q]) =
  Call (Prim PrimOr) [negateBoolExpr p, negateBoolExpr q]

negateExpr (Call (Prim PrimOr) [p,q]) =
  Call (Prim PrimAnd) [negateBoolExpr p, negateBoolExpr q]

negateExpr x = Call (Prim PrimLogNot) [x]

negateBoolExpr (Call (Prim PrimLogNot) [x]) = x
negateBoolExpr x = negateExpr x


decompileReady (ZInstr a b True (Const _ _ x : args) c d) | x /= 0 =
  decompileReady (ZInstr a b False (makeVar x : args) c d)

decompileReady (ZInstr _ OPush False [x] _ _) =
  Just (ZPush x)

decompileReady (ZInstr _ (OJumpIf PrimTrue) False _ _ (Just (True,dest))) =
  Just (ZJump dest)

decompileReady (ZInstr _ (OJumpIf p) False args Nothing (Just (dir,target))) =
  Just (ZJCond (Call (PrimJump p dir) args) target)

-- We occasionally get a jump to the immediately following location...
decompileReady (ZInstr _ (OJumpIf p) False args Nothing Nothing) =
  Just (ZIfThenElse (Call (PrimJump p True) args) [] [])

decompileReady (ZInstr _ (OPrim p) False args store Nothing) =
  Just (pushOrStoreOrEval store (Call (Prim p) args))

-- Annoying exceptions: get_child and get_sibling
-- both store and branch...
decompileReady x@(ZInstr _ (OPrim p) False args store (Just (dir,target))) =
  case pushOrStoreOrEval store (Call (Prim p) args) of
    ZEval expr -> compareWith0 expr
    ZPush expr -> compareWith0 expr -- in V3 files, when the result isn't used
    _          -> Nothing
  where
    compareWith0 expr =
      Just (ZJCond (Call (PrimJump PrimEq (not dir)) [expr, Const 0 TypeObject 0]) target)

decompileReady (ZInstr _ OReturn False [x] _ _) =
  Just (ZReturn x)

decompileReady (ZInstr _ OLoad False [var] store Nothing) | var /= SP =
  Just (pushOrStoreOrEval store var)

decompileReady (ZInstr _ OStore False [var,expr] Nothing Nothing) | var /= SP =
  Just (ZEval (Assignment var expr))

decompileReady x = Nothing

{-
  | n == 0  = ZSpecial "style roman"
  | n == 1  = ZSpecial "style reverse"
  | n == 2  = ZSpecial "style bold"
  | n == 4  = ZSpecial "style underline"
-}


ready = not . hasSP
hasSP = any isSP
isSP SP = True
isSP _  = False

substSP val args =
  left ++ val : right where (left,(_:right)) = break isSP args

pushOrStoreOrEval Nothing    expr = ZEval expr
pushOrStoreOrEval (Just SP)  expr = ZPush expr
pushOrStoreOrEval (Just var) expr = ZEval (Assignment var expr)
