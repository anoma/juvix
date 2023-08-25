module Juvix.Compiler.Backend.VampIR.Pretty.Base where

import Data.ByteString.UTF8 qualified as UTF8
import Data.FileEmbed qualified as FE
import Juvix.Compiler.Backend.VampIR.Language
import Juvix.Compiler.Backend.VampIR.Pretty.Keywords
import Juvix.Compiler.Backend.VampIR.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Data.NameKind

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

doc :: (PrettyCode c) => Options -> c -> Doc Ann
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

ppPub :: Text -> Sem r (Doc Ann)
ppPub p = return $ "pub " <> pretty p <> semi

instance PrettyCode Program where
  ppCode Program {..} = do
    pubs <- mapM ppPub _programPublicInputs
    fns <- mapM ppCode _programFunctions
    eqns <- mapM ppEquation _programFunctions
    bits <- asks (^. optIntegerBits)
    return $ vsep pubs <> line <> line <> pretty (vampIRDefs bits) <> line <> line <> vsep fns <> line <> line <> vsep eqns

--------------------------------------------------------------------------------
-- VampIR definitions
--------------------------------------------------------------------------------

vampIRDefs :: Int -> String
vampIRDefs bits =
  "def integerBits = "
    <> show bits
    <> ";\n"
    <> UTF8.toString $(FE.makeRelativeToProject "runtime/src/vampir/stdlib.pir" >>= FE.embedFile)

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
