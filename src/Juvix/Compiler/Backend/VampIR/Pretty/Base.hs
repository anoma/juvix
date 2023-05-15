module Juvix.Compiler.Backend.VampIR.Pretty.Base where

import Juvix.Compiler.Backend.VampIR.Language
import Juvix.Compiler.Backend.VampIR.Pretty.Keywords
import Juvix.Compiler.Backend.VampIR.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Data.NameKind

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

doc :: PrettyCode c => Options -> c -> Doc Ann
doc opts x =
  run $
    runReader opts $
      ppCode x

ppName :: NameKind -> Text -> Sem r (Doc Ann)
ppName k n = return $ annotate (AnnKind k) (pretty n)

instance PrettyCode Var where
  ppCode Var {..} = ppName KNameLocal _varName

instance PrettyCode OpCode where
  ppCode = \case
    OpAdd -> return kwAdd
    OpSub -> return kwSub
    OpMul -> return kwMul
    OpDiv -> return kwDiv
    OpMod -> return kwMod
    OpEq -> return kwEqual
    OpLt -> return kwLessThan
    OpLe -> return kwLessOrEqual

instance PrettyCode Binop where
  ppCode Binop {..} = do
    op <- ppCode _binopOp
    l <- ppArg _binopLeft
    r <- ppArg _binopRight
    return $ op <+> l <+> r

instance PrettyCode IfThenElse where
  ppCode IfThenElse {..} = do
    c <- ppArg _ifThenElseCondition
    l <- ppArg _ifThenElseBranchTrue
    r <- ppArg _ifThenElseBranchFalse
    return $ kwIf <+> c <+> l <+> r

instance PrettyCode Expression where
  ppCode = \case
    ExpressionVar x -> ppCode x
    ExpressionConstant i -> return $ annotate AnnLiteralInteger (pretty i)
    ExpressionBinop x -> ppCode x
    ExpressionIfThenElse x -> ppCode x
    ExpressionFail -> return $ annotate AnnLiteralInteger "0"

instance PrettyCode LocalDef where
  ppCode LocalDef {..} = do
    n <- ppName KNameLocal _localDefName
    v <- ppCode _localDefValue
    return $ kwDef <+> n <+> kwEq <+> v <> semi <> line

instance PrettyCode Function where
  ppCode Function {..} = do
    n <- ppName KNameFunction _functionName
    args <- mapM (ppName KNameLocal) _functionArguments
    defs <- mapM ppCode _functionLocalDefs
    e <- ppCode _functionExpression
    return $ kwDef <+> hsep (n : args) <+> kwEq <+> bracesIndent (vsep defs <> line <> e) <> semi

ppEquation :: Function -> Sem r (Doc Ann)
ppEquation Function {..} = do
  let n = length _functionArguments
      args = if n == 1 then ["in"] else map (\k -> "in" <> show k) [1 .. n]
  fn <- ppName KNameFunction _functionName
  return $ fn <+> hsep args <+> kwEq <+> "out" <> semi

instance PrettyCode Program where
  ppCode Program {..} = do
    fns <- mapM ppCode _programFunctions
    eqns <- mapM ppEquation _programFunctions
    return $ vsep vampIRDefs <> line <> line <> vsep fns <> line <> line <> vsep eqns

--------------------------------------------------------------------------------
-- VampIR definitions
--------------------------------------------------------------------------------

vampIRDefs :: [Doc Ann]
vampIRDefs =
  [ "def add x y = x + y;",
    "def sub x y = x - y;",
    "def mul x y = x * y;",
    "def isZero x = {def xi = fresh (1 | x); x * (1 - xi * x) = 0; 1 - xi * x};",
    "def equal x y = isZero (x - y);",
    "def bool x = {x * (x - 1) = 0; x};",
    "def range32 a = {def a0 = bool (fresh ((a \\ 1) % 2)); def a1 = bool (fresh ((a \\ 2) % 2)); def a2 = bool (fresh ((a \\ 4) % 2)); def a3 = bool (fresh ((a \\ 8) % 2)); def a4 = bool (fresh ((a \\ 16) % 2)); def a5 = bool (fresh ((a \\ 32) % 2)); def a6 = bool (fresh ((a \\ 64) % 2)); def a7 = bool (fresh ((a \\ 128) % 2)); def a8 = bool (fresh ((a \\ 256) % 2)); def a9 = bool (fresh ((a \\ 512) % 2)); def a10 = bool (fresh ((a \\ 1024) % 2)); def a11 = bool (fresh ((a \\ 2048) % 2)); def a12 = bool (fresh ((a \\ 4096) % 2)); def a13 = bool (fresh ((a \\ 8192) % 2)); def a14 = bool (fresh ((a \\ 16384) % 2)); def a15 = bool (fresh ((a \\ 32768) % 2)); def a16 = bool (fresh ((a \\ 65536) % 2)); def a17 = bool (fresh ((a \\ 131072) % 2)); def a18 = bool (fresh ((a \\ 262144) % 2)); def a19 = bool (fresh ((a \\ 524288) % 2)); def a20 = bool (fresh ((a \\ 1048576) % 2)); def a21 = bool (fresh ((a \\ 2097152) % 2)); def a22 = bool (fresh ((a \\ 4194304) % 2)); def a23 = bool (fresh ((a \\ 8388608) % 2)); def a24 = bool (fresh ((a \\ 16777216) % 2)); def a25 = bool (fresh ((a \\ 33554432) % 2)); def a26 = bool (fresh ((a \\ 67108864) % 2)); def a27 = bool (fresh ((a \\ 134217728) % 2)); def a28 = bool (fresh ((a \\ 268435456) % 2)); def a29 = bool (fresh ((a \\ 536870912) % 2)); def a30 = bool (fresh ((a \\ 1073741824) % 2)); def a31 = bool (fresh ((a \\ 2147483648) % 2)); a = ((((((((((((((((((((((((((((((a0 + (2 * a1)) + (4 * a2)) + (8 * a3)) + (16 * a4)) + (32 * a5)) + (64 * a6)) + (128 * a7)) + (256 * a8)) + (512 * a9)) + (1024 * a10)) + (2048 * a11)) + (4096 * a12)) + (8192 * a13)) + (16384 * a14)) + (32768 * a15)) + (65536 * a16)) + (131072 * a17)) + (262144 * a18)) + (524288 * a19)) + (1048576 * a20)) + (2097152 * a21)) + (4194304 * a22)) + (8388608 * a23)) + (16777216 * a24)) + (33554432 * a25)) + (67108864 * a26)) + (134217728 * a27)) + (268435456 * a28)) + (536870912 * a29)) + (1073741824 * a30)) + (2147483648 * a31); (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, ())};",
    "def intrange32 a = {range32 (a + 2147483648)};",
    "def negative32 a = {def (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, ()) = intrange32 a; a31};",
    "def isNegative x = negative32 x;",
    "def lessThan x y = isNegative (x - y);",
    "def lessOrEqual x y = lessThan x (y + 1);",
    "def divRem a b = {def q = fresh (a\\b); def r = fresh (a%b); isNegative r = 0; lessThan r b = 1; a = b * q + r; (q, r) };",
    "def fst (x, y) = x;",
    "def snd (x, y) = y;",
    "def div x y = fst (divRem x y);",
    "def rem x y = snd (divRem x y);",
    "def if b x y = b * x + (1 - b) * y;"
  ]

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

ppArg ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  a ->
  Sem r (Doc Ann)
ppArg = ppRightExpression appFixity

ppRightExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression ::
  (HasAtomicity a, PrettyCode a, Member (Reader Options) r) =>
  (Fixity -> Bool) ->
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLRExpression associates fixlr e =
  parensIf (atomParens associates (atomicity e) fixlr)
    <$> ppCode e
