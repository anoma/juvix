module Juvix.Compiler.Backend.VampIR.Pretty.Base where

import Juvix.Compiler.Backend.VampIR.Language
import Juvix.Compiler.Backend.VampIR.Pretty.Keywords
import Juvix.Compiler.Backend.VampIR.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Data.NameKind

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

doc :: (HasAtomicity c, PrettyCode c) => Options -> c -> Doc Ann
doc opts x =
  run $
    runReader opts $
      case atomicity x of
        Atom -> ppCode x
        Aggregate _ -> parens <$> ppCode x

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
    OpEq -> return kwEqual
    OpLt -> return kwLessThan
    OpAnd -> return kwAnd
    OpOr -> return kwOr

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
    return $ kwDef <+> n <+> kwEq <+> v <+> semi <> line

instance PrettyCode Function where
  ppCode Function {..} = do
    n <- ppName KNameFunction _functionName
    args <- mapM (ppName KNameLocal) _functionArguments
    defs <- mapM ppCode _functionLocalDefs
    e <- ppCode _functionExpression
    return $ kwDef <+> hsep (n : args) <+> kwEq <+> bracesIndent (vsep defs <> line <> e) <> semi

instance PrettyCode VampIR where
  ppCode VampIR {..} = do
    fns <- mapM ppCode _vampIRFunctions
    return $ vsep vampIRDefs <> line <> line <> vsep fns

--------------------------------------------------------------------------------
-- VampIR definitions
--------------------------------------------------------------------------------

vampIRDefs :: [Doc Ann]
vampIRDefs =
  [ "def add x y = x + y;",
    "def sub x y = x - y;",
    "def mul x y = x * y;",
    "def div x y = x / y;",
    "def isZero x = {def xi = fresh (1 | x); x * (1 - xi * x) = 0; 1 - xi * x};",
    "def equal x y = isZero (x - y);",
    "def decomp_rec bits a = {def a0 = fresh (a%2); isBool a0; def a1 = fresh (a\2); a = a0 + 2*a1; (a0 : bits a1)};",
    "def decomp n = iter n decomp_rec (fun x {[]});",
    "def decompInt n x = decomp n (x + 2^n);",
    "def lessThan x y = last (decompInt 32 (x - y));",
    "def negb x = 1 - x;",
    "def andb x y = x * y;",
    "def orb x y = negb (andb (negb x) (negb y));",
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
