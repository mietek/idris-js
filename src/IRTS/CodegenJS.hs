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
      prelude ++ "\n" ++
      concatMap doCodegen (simpleDecls ci) ++
      name (sMN 0 "runMain") ++ "();\n"

doCodegen :: (Name, SDecl) -> String
doCodegen (n, SFun _ args _ def) =
    cgFun n args def

name :: Name -> String
name n =
    "idris_" ++ concatMap char (showCG n)
  where
    char x | isAlpha x || isDigit x = [x]
           | otherwise              = "_" ++ show (fromEnum x) ++ "_"

cgFun :: Name -> [Name] -> SExp -> String
cgFun n args def =
    "function " ++ name n ++ "(" ++ showSep ", " (map arg [0..a]) ++ ") {" ++ cr 1 ++
    cgArgs a ++
    cgBody 1 ret def ++ "\n}\n\n"
  where
    a     = length args
    arg i = "arg" ++ show i
    ret s = "return " ++ s ++ ";"

cgArgs :: Int -> String
cgArgs 0 = ""
cgArgs a = showSep (cr 1) (map arg [0..a]) ++ cr 1
  where
    arg i = "var " ++ loc i ++ " = arg" ++ show i ++ ";"

cr :: Int -> String
cr l = "\n" ++ concat (replicate l "  ")

loc :: Int -> String
loc i = "loc" ++ show i

cgVar :: LVar -> String
cgVar (Loc i)  = loc i
cgVar (Glob n) = name n

cgBody :: Int -> (String -> String) -> SExp -> String
cgBody _ ret (SV (Glob f))          = ret $ name f ++ "()"
cgBody _ ret (SV (Loc i))           = ret $ loc i
cgBody _ ret (SApp _ f vs)          = ret $ name f ++ "(" ++ showSep ", " (map cgVar vs) ++ ")"
cgBody l ret (SLet (Loc i) e1 e2)   = cgBody l (\s -> "var " ++ loc i ++ " = " ++ s ++ ";") e1 ++ cr l ++
                                      cgBody l ret e2
-- cgBody l r (SUpdate _ e)
-- cgBody l r (SProj v i)
cgBody _ ret (SCon _ t _ vs)        = ret $ "[" ++ showSep ", " (show t : map cgVar vs) ++ "]"
cgBody l ret (SCase _ v as)         = cgSwitch l ret v as
cgBody l ret (SChkCase v as)        = cgSwitch l ret v as
cgBody _ ret (SConst c)             = ret $ cgConst c
cgBody _ ret (SOp o vs)             = ret $ cgOp o (map cgVar vs)
cgBody _ ret SNothing               = ret "0"
cgBody l _ s                        = "// " ++ show s ++ cr l

cgSwitch :: Int -> (String -> String) -> LVar -> [SAlt] -> String
cgSwitch l ret v cs = let
                        v'  = cgVar v
                        v'' = if any isConCase cs then v' ++ "[0]" else v'
                      in
                        "switch (" ++ v'' ++ ") {" ++ cr (l + 1) ++
                        showSep (cr (l + 2) ++ "break;" ++ cr (l + 1)) (map (cgCase (l + 2) ret v') cs) ++ cr l ++
                        "}"
  where
    isConCase (SConCase _ _ _ _ _) = True
    isConCase _                    = False

cgCase :: Int -> (String -> String) -> String -> SAlt -> String
cgCase l ret _ (SDefaultCase e)        = "default:" ++ cr l ++
                                         cgBody l ret e
cgCase l ret _ (SConstCase t e)        = "case " ++ show t ++ ":" ++ cr l ++
                                         cgBody l ret e
cgCase l ret v (SConCase i0 t _ ns0 e) = "case " ++ show t ++ ":" ++
                                         project 1 i0 ns0 ++ cr l ++
                                         cgBody l ret e
  where
    project :: Int -> Int -> [Name] -> String
    project _ _ []       = ""
    project k i (_ : ns) = cr l ++ "var " ++ loc i ++ " = " ++ v ++ "[" ++ show k ++ "];" ++
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
cgOp (LPlus  (ATInt _)) [n, m] = "(" ++ n ++ " + " ++ m ++ ")"
cgOp (LMinus (ATInt _)) [n, m] = "(" ++ n ++ " - " ++ m ++ ")"
cgOp (LTimes (ATInt _)) [n, m] = "(" ++ n ++ " * " ++ m ++ ")"
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
cgOp (LSLt   (ATInt _)) [n, m] = "(" ++ n ++ " < "  ++ m ++ ")"
cgOp (LSLe   (ATInt _)) [n, m] = "(" ++ n ++ " <= " ++ m ++ ")"
cgOp (LSGt   (ATInt _)) [n, m] = "(" ++ n ++ " > "  ++ m ++ ")"
cgOp (LSGe   (ATInt _)) [n, m] = "(" ++ n ++ " >= " ++ m ++ ")"
cgOp (LSExt  _ _)       [n]    = n
-- cgOp LZExt
-- cgOp LTrunc
cgOp LStrConcat         [s, t] = "(" ++ s ++ " + "   ++ t ++ ")"
cgOp LStrLt             [s, t] = "(" ++ s ++ " < "   ++ t ++ ")"
cgOp LStrEq             [s, t] = "(" ++ s ++ " === " ++ t ++ " ? 1 : 0)"
cgOp LStrLen            [s]    = s ++ ".length"
-- cgOp LIntFloat
-- cgOp LFloatInt
cgOp (LIntStr _)        [x]    = "('' + " ++ x ++ ")"
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
cgOp LStrRev            [s]    = "idris_reverseStr(" ++ s ++ ")"
-- cgOp LReadStr
cgOp LWriteStr          [_, s] = "idris_writeStr(" ++ s ++ ")"
-- cgOp LSystemInfo
-- cgOp LFork
-- cgOp LPar
-- cgOp LExternal
-- cgOp LNoOp
-- cgOp o _                       = error $ "Operator " ++ show o ++ " is not supported"
cgOp o _                       = "idris_error('Operator " ++ show o ++ " is not supported')"
