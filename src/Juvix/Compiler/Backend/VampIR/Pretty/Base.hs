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
    return $ kwDef <+> n <+> kwEq <+> v <> semi

instance PrettyCode Function where
  ppCode Function {..} = do
    n <- ppName KNameFunction _functionName
    args <- mapM (ppName KNameLocal) _functionArguments
    defs <- mapM ppCode _functionLocalDefs
    e <- ppCode _functionExpression
    return $ kwDef <+> hsep (n : args) <+> kwEq <+> bracesIndent (vsep defs <> line <> e) <> semi

ppEquation :: Function -> Sem r (Doc Ann)
ppEquation Function {..} = do
  let args = map (\arg -> "(" <> pretty arg <> " + 0)") _functionInputs
      out = pretty _functionOutput
  fn <- ppName KNameFunction _functionName
  return $ fn <+> hsep args <+> kwEq <+> "(" <> out <> " + 0)" <> semi

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
    "def isBool x = (x * (x - 1) = 0);",
    "def isNegative a = {def e = 2^30; def b = a + e; def b0 = fresh (b % e); def b1 = fresh (b \\ e); isBool b1; b = b0 + e * b1; 1 - b1};",
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
