module IRTS.CodegenJS (codegenJS) where

import Data.Char
import qualified Data.IntMap.Strict as M
import IRTS.CodegenCommon
import IRTS.CodegenUtils
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Paths_idris_js


codegenJS :: CodeGenerator
codegenJS ci = do
    preludeName <- getDataFileName "prelude.js"
    prelude <- readFile preludeName
    let tm = findTags ci
    writeFile (outputFile ci) $
      prelude ++ "\n\n" ++
      cgTags tm ++ "\n\n" ++
      concatMap (doCodegen tm) (simpleDecls ci) ++
      name (sMN 0 "runMain") ++ "();\n"


cgTags :: TagMap -> String
cgTags tm = showSep "\n" (map tag (M.toAscList tm)) ++ "\n" ++
            "_AP = " ++ show (M.size tm) ++ ";\n"
  where
    tag (t, ap) = "_A[" ++ show ap ++ "] = " ++ show t ++ ";"


doCodegen :: TagMap -> (Name, SDecl) -> String
doCodegen tm (n, SFun _ args _ e) =
    cgFun tm n (length args) e


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


cgFun :: TagMap -> Name -> Int -> SExp -> String
cgFun tm n argCount e =
    "function " ++ name n ++ "() {" ++ cr 1 ++
    pushFrame ++
    moveArgs ++
    sizeFrame ++
    cgBody tm 1 ret e ++
    popFrame ++ "\n}\n\n\n"
  where
    frameSize = max argCount (locCountBody e)
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


cgBody :: TagMap -> Int -> String -> SExp -> String
cgBody _  l r (SV (Glob f))        = name f ++ "();" ++
                                     cgRet l r
cgBody _  _ r (SV (Loc i))         = r ++ " = " ++ loc i ++ ";"
cgBody _  l r (SApp _ f vs)        = name f ++ "(" ++ showSep ", " (map cgVar vs) ++ ");" ++
                                     cgRet l r
cgBody tm l r (SLet (Loc i) e1 e2) = cgBody tm l (loc i) e1 ++ cr l ++
                                     cgBody tm l r e2
-- cgBody tm l r (SUpdate _ e)
-- cgBody tm l r (SProj v i)
cgBody tm _ r (SCon _ t _ [])      = r ++ " = " ++ show (tm M.! t) ++ ";"
cgBody _  l r (SCon _ t _ vs)      = makeArray l r (show t : map cgVar vs)
cgBody tm l r (SCase _ v cs)       = cgSwitch tm l r v cs
cgBody tm l r (SChkCase v cs)      = cgSwitch tm l r v cs
cgBody _  _ r (SConst c)           = r ++ " = " ++ cgConst c ++ ";"
cgBody _  _ r (SOp o vs)           = cgOp r o (map cgVar vs)
cgBody _  _ r SNothing             = r ++ " = 0;"
-- cgBody tm l r (SError x)
cgBody _  _ _ x                    = error $ "Expression " ++ show x ++ " is not supported"


makeArray :: Int -> String -> [String] -> String
makeArray l r args =
    makeElements ++
    r ++ " = _AP;" ++
    -- cr l ++ "console.log(_AP, " ++ showSep (", ") args ++ ");" ++
    pushArray
  where
    argCount = length args
    makeElements | argCount == 0 = ""
                 | otherwise     = showSep (cr l) (map makeElement (zip [1..argCount] args)) ++ cr l
    makeElement (1, arg) = "_A[_AP] = " ++ arg ++ ";"
    makeElement (i, arg) = "_A[_AP + " ++ show (i - 1) ++ "] = " ++ arg ++ ";"
    pushArray    | argCount == 0 = ""
                 | otherwise     = cr l ++ "_AP += " ++ show argCount ++ ";"


cgRet :: Int -> String -> String
cgRet l r | r == ret  = ""
          | otherwise = cr l ++ r ++ " = " ++ ret ++ ";"


cgSwitch :: TagMap -> Int -> String -> LVar -> [SAlt] -> String
cgSwitch tm l r v cs =
    let
      v'  = cgVar v
      v'' = if any isConCase cs then "_A[" ++ v' ++ "]" else v'
    in
      "switch (" ++ v'' ++ ") {" ++ cr (l + 1) ++
      showSep (cr (l + 2) ++ "break;" ++ cr (l + 1)) (map (cgCase tm (l + 2) r v') cs) ++ cr l ++
      "}"
  where
    isConCase (SConCase _ _ _ _ _) = True
    isConCase _                    = False


cgCase :: TagMap -> Int -> String -> String -> SAlt -> String
cgCase tm l r _ (SDefaultCase e)        = "default:" ++ cr l ++
                                          cgBody tm l r e
cgCase tm l r _ (SConstCase t e)        = "case " ++ show t ++ ":" ++ cr l ++
                                          cgBody tm l r e
cgCase tm l r v (SConCase i0 t _ ns0 e) = "case " ++ show t ++ ":" ++ cr l ++
                                          project 1 i0 ns0 ++
                                          cgBody tm l r e
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


cgOp :: String -> PrimFn -> [String] -> String
cgOp r (LPlus  (ATInt _)) [n, m] = r ++ " = "  ++ n ++ " + "   ++ m ++ ";"
cgOp r (LMinus (ATInt _)) [n, m] = r ++ " = "  ++ n ++ " - "   ++ m ++ ";"
cgOp r (LTimes (ATInt _)) [n, m] = r ++ " = "  ++ n ++ " * "   ++ m ++ ";"
-- cgOp r LUDiv
-- cgOp r LSDiv
-- cgOp r LURem
-- cgOp r LSRem
-- cgOp r LAnd
-- cgOp r LOr
-- cgOp r LXOr
-- cgOp r LCompl
-- cgOp r LSHL
-- cgOp r LLSHR
-- cgOp r LASHR
cgOp r (LEq    (ATInt _)) [n, m] = r ++ " = (" ++ n ++ " === " ++ m ++ ") ? 1 : 0;"
-- cgOp r LLt
-- cgOp r LLe
-- cgOp r LGt
-- cgOp r LGe
cgOp r (LSLt   (ATInt _)) [n, m] = r ++ " = (" ++ n ++ " < "   ++ m ++ ") ? 1 : 0;"
cgOp r (LSLe   (ATInt _)) [n, m] = r ++ " = (" ++ n ++ " <= "  ++ m ++ ") ? 1 : 0;"
cgOp r (LSGt   (ATInt _)) [n, m] = r ++ " = (" ++ n ++ " > "   ++ m ++ ") ? 1 : 0;"
cgOp r (LSGe   (ATInt _)) [n, m] = r ++ " = (" ++ n ++ " >= "  ++ m ++ ") ? 1 : 0;"
cgOp r (LSExt  _ _)       [n]    = r ++ " = "  ++ n ++ ";"
-- cgOp r LZExt
-- cgOp r LTrunc
cgOp r LStrConcat         [s, t] = r ++ " = "  ++ s ++ " + "   ++ t ++ ";"
cgOp r LStrLt             [s, t] = r ++ " = (" ++ s ++ " < "   ++ t ++ ") ? 1 : 0;"
cgOp r LStrEq             [s, t] = r ++ " = (" ++ s ++ " === " ++ t ++ ") ? 1 : 0;"
cgOp r LStrLen            [s]    = r ++ " = "  ++ s ++ ".length;"
-- cgOp r LIntFloat
-- cgOp r LFloatInt
cgOp r (LIntStr _)        [n]    = r ++ " = '' + " ++ n ++ ";"
-- cgOp r LStrInt
-- cgOp r LFloatStr
-- cgOp r LStrFloat
-- cgOp r LChInt
-- cgOp r LIntCh
-- cgOp r LBitCast
-- cgOp r LFExp
-- cgOp r LFLog
-- cgOp r LFSin
-- cgOp r LFCos
-- cgOp r LFTan
-- cgOp r LFASin
-- cgOp r LFACos
-- cgOp r LFATan
-- cgOp r LFSqrt
-- cgOp r LFFloor
-- cgOp r LFCeil
-- cgOp r LFNegate
-- cgOp r LStrHead
-- cgOp r LStrTail
-- cgOp r LStrCons
-- cgOp r LStrIndex
-- cgOp r LStrRev
-- cgOp r LReadStr
cgOp _ LWriteStr          [_, s] = "console.log(" ++ s ++ ");"
-- cgOp r LSystemInfo
-- cgOp r LFork
-- cgOp r LPar
-- cgOp r LExternal
-- cgOp r LNoOp
-- cgOp _ o _                       = error $ "Operator " ++ show o ++ " is not supported"
cgOp _ o _                       = "console.error('Operator " ++ show o ++ " is not supported');"
