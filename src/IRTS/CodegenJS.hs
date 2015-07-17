module IRTS.CodegenJS (codegenJS) where

import Data.Char
import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Paths_idris_js

codegenJS :: CodeGenerator
codegenJS ci = do
    preludeName <- getDataFileName "prelude.js"
    prelude <- readFile preludeName
    writeFile (outputFile ci) $
      prelude ++ "\n\n" ++
      concatMap doCodegen (simpleDecls ci) ++
      name (sMN 0 "runMain") ++ "();\n"

doCodegen :: (Name, SDecl) -> String
doCodegen (n, SFun _ args _ e) =
    cgFun n (length args) e

name :: Name -> String
name n =
    "idris_" ++ concatMap char (showCG n)
  where
    char x | isAlpha x || isDigit x = [x]
           | otherwise              = "_" ++ show (fromEnum x) ++ "_"

cr :: Int -> String
cr l = "\n" ++ concat (replicate l "  ")

loc :: Int -> String
loc 0 = "_S[_SP]"
loc i = "_S[_SP + " ++ show i ++ "]"

ret :: String
ret = "_R"

cgVar :: LVar -> String
cgVar (Loc i)  = loc i
cgVar (Glob n) = name n

cgFun :: Name -> Int -> SExp -> String
cgFun n argCount e =
    "function " ++ name n ++ "() {" ++ cr 1 ++
    pushFrame ++
    moveArgs ++
    sizeFrame ++
    cgBody 1 ret e ++
    popFrame ++ "\n}\n\n\n"
  where
    frameSize = max argCount (measureBody e)
    pushFrame | frameSize == 0 = ""
              | otherwise      = "_PSP[_SR] = _SP; _SP = _SQ; _SR += 1;" ++ cr 1
    moveArgs  | argCount == 0  = ""
              | otherwise      = showSep (cr 1) (map moveArg [1..argCount]) ++ cr 1
    moveArg 1 = "_S[_SP] = arguments[0];"
    moveArg i = "_S[_SP + " ++ show (i - 1) ++ "] = arguments[" ++ show (i - 1) ++ "];"
    sizeFrame | frameSize == 0 = ""
              | otherwise      = "_SQ = _SP + " ++ show frameSize ++ ";" ++ cr 1
    popFrame  | frameSize == 0 = ""
              | otherwise      = cr 1 ++ "_SQ = _SP; _SR -= 1; _SP = _PSP[_SR];"

measureBody :: SExp -> Int
measureBody (SV (Glob _))        = 0
measureBody (SV (Loc i))         = i
measureBody (SApp _ _ _)         = 0
measureBody (SLet (Loc i) e1 e2) = max i (max (measureBody e1) (measureBody e2))
-- measureBody (SUpdate _ _)
-- measureBody (SProj _ _)
measureBody (SCon _ _ _ _)       = 0
measureBody (SCase _ _ cs)       = measureSwitch cs
measureBody (SChkCase _ cs)      = measureSwitch cs
measureBody (SConst _)           = 0
measureBody (SOp _ _)            = 0
measureBody SNothing             = 0
-- measureBody (SError x)
measureBody x                    = error $ "Expression " ++ show x ++ " is not supported"

measureSwitch :: [SAlt] -> Int
measureSwitch cs = maximum (map measureCase cs)

measureCase :: SAlt -> Int
measureCase (SDefaultCase e)      = measureBody e
measureCase (SConstCase _ e)      = measureBody e
measureCase (SConCase _ _ _ [] e) = measureBody e
measureCase (SConCase i _ _ ns e) = max (i + length ns - 1) (measureBody e)

cgBody :: Int -> String -> SExp -> String
cgBody l r (SV (Glob f))        = name f ++ "();" ++
                                  cgRet l r
cgBody _ r (SV (Loc i))         = r ++ " = " ++ loc i ++ ";"
cgBody l r (SApp _ f vs)        = name f ++ "(" ++ showSep ", " (map cgVar vs) ++ ");" ++
                                  cgRet l r
cgBody l r (SLet (Loc i) e1 e2) = cgBody l (loc i) e1 ++ cr l ++
                                  cgBody l r e2
-- cgBody l r (SUpdate _ e)
-- cgBody l r (SProj v i)
cgBody l r (SCon _ t _ vs)      = "idris_makeArray(" ++ showSep ", " (show t : map cgVar vs) ++ ");" ++
                                  cgRet l r
cgBody l r (SCase _ v cs)       = cgSwitch l r v cs
cgBody l r (SChkCase v cs)      = cgSwitch l r v cs
cgBody _ r (SConst c)           = r ++ " = " ++ cgConst c ++ ";"
cgBody _ r (SOp o vs)           = r ++ " = " ++ cgOp o (map cgVar vs) ++ ";"
cgBody _ r SNothing             = r ++ " = 0;"
-- cgBody l r (SError x)
cgBody _ _ x                    = error $ "Expression " ++ show x ++ " is not supported"

cgRet :: Int -> String -> String
cgRet l r | r == ret  = ""
          | otherwise = cr l ++ r ++ " = " ++ ret ++ ";"

cgSwitch :: Int -> String -> LVar -> [SAlt] -> String
cgSwitch l r v cs =
    let
      v'  = cgVar v
      v'' = if any isConCase cs then "_A[" ++ v' ++ "]" else v'
    in
      "switch (" ++ v'' ++ ") {" ++ cr (l + 1) ++
      showSep (cr (l + 2) ++ "break;" ++ cr (l + 1)) (map (cgCase (l + 2) r v') cs) ++ cr l ++
      "}"
  where
    isConCase (SConCase _ _ _ _ _) = True
    isConCase _                    = False

cgCase :: Int -> String -> String -> SAlt -> String
cgCase l r _ (SDefaultCase e)        = "default:" ++ cr l ++
                                       cgBody l r e
cgCase l r _ (SConstCase t e)        = "case " ++ show t ++ ":" ++ cr l ++
                                       cgBody l r e
cgCase l r v (SConCase i0 t _ ns0 e) = "case " ++ show t ++ ":" ++ cr l ++
                                       project 1 i0 ns0 ++
                                       cgBody l r e
  where
    project :: Int -> Int -> [Name] -> String
    project _ _ []       = ""
    project k i (_ : ns) = loc i ++ " = _A[" ++ v ++ " + " ++ show k ++ "];" ++ cr l ++
                           project (k + 1) (i + 1) ns

cgConst :: Const -> String
cgConst (I i)             = show i
cgConst (BI i)            = show i
-- cgConst (Ch c)
cgConst (Str s)           = "'" ++ s ++ "'"
cgConst TheWorld          = "0"
cgConst x | isTypeConst x = "0"
          | otherwise     = error $ "Constant " ++ show x ++ " is not supported"

cgOp :: PrimFn -> [String] -> String
cgOp (LPlus  (ATInt _)) [n, m] = "(" ++ n ++ " + "   ++ m ++ ")"
cgOp (LMinus (ATInt _)) [n, m] = "(" ++ n ++ " - "   ++ m ++ ")"
cgOp (LTimes (ATInt _)) [n, m] = "(" ++ n ++ " * "   ++ m ++ ")"
-- cgOp LUDiv
-- cgOp LSDiv
-- cgOp LURem
-- cgOp LSRem
-- cgOp LAnd
-- cgOp LOr
-- cgOp LXOr
-- cgOp LCompl
-- cgOp LSHL
-- cgOp LLSHR
-- cgOp LASHR
cgOp (LEq    (ATInt _)) [n, m] = "(" ++ n ++ " === " ++ m ++ " ? 1 : 0)"
-- cgOp LLt
-- cgOp LLe
-- cgOp LGt
-- cgOp LGe
cgOp (LSLt   (ATInt _)) [n, m] = "(" ++ n ++ " < "   ++ m ++ ")"
cgOp (LSLe   (ATInt _)) [n, m] = "(" ++ n ++ " <= "  ++ m ++ ")"
cgOp (LSGt   (ATInt _)) [n, m] = "(" ++ n ++ " > "   ++ m ++ ")"
cgOp (LSGe   (ATInt _)) [n, m] = "(" ++ n ++ " >= "  ++ m ++ ")"
cgOp (LSExt  _ _)       [n]    =        n
-- cgOp LZExt
-- cgOp LTrunc
cgOp LStrConcat         [s, t] = "(" ++ s ++ " + "   ++ t ++ ")"
cgOp LStrLt             [s, t] = "(" ++ s ++ " < "   ++ t ++ ")"
cgOp LStrEq             [s, t] = "(" ++ s ++ " === " ++ t ++ " ? 1 : 0)"
cgOp LStrLen            [s]    =        s ++ ".length"
-- cgOp LIntFloat
-- cgOp LFloatInt
cgOp (LIntStr _)        [n]    = "('' + " ++ n ++ ")"
-- cgOp LStrInt
-- cgOp LFloatStr
-- cgOp LStrFloat
-- cgOp LChInt
-- cgOp LIntCh
-- cgOp LBitCast
-- cgOp LFExp
-- cgOp LFLog
-- cgOp LFSin
-- cgOp LFCos
-- cgOp LFTan
-- cgOp LFASin
-- cgOp LFACos
-- cgOp LFATan
-- cgOp LFSqrt
-- cgOp LFFloor
-- cgOp LFCeil
-- cgOp LFNegate
-- cgOp LStrHead
-- cgOp LStrTail
-- cgOp LStrCons
-- cgOp LStrIndex
-- cgOp LStrRev
-- cgOp LReadStr
cgOp LWriteStr          [_, s] = "idris_writeStr(" ++ s ++ ")"
-- cgOp LSystemInfo
-- cgOp LFork
-- cgOp LPar
-- cgOp LExternal
-- cgOp LNoOp
-- cgOp o _                       = error $ "Operator " ++ show o ++ " is not supported"
cgOp o _                       = "idris_error('Operator " ++ show o ++ " is not supported')"
