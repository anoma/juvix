module Juvix.Compiler.Nockma.Evaluator.Crumbs where

import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty.Base
import Juvix.Prelude hiding (Atom)

data EvalCrumb
  = EvalCrumbStdlibCallArgs CrumbStdlibCallArgs
  | EvalCrumbOperator CrumbOperator
  | EvalCrumbAutoCons CrumbAutoCons

newtype EvalCtx = EvalCtx
  { _evalStack :: [EvalCrumb]
  }

topEvalCtx :: Sem (Reader EvalCtx ': r) a -> Sem r a
topEvalCtx = runReader (EvalCtx [])

newtype CrumbStdlibCallArgs = CrumbStdlibCallArgs
  { _crumbStdlibCallArgsFunction :: StdlibFunction
  }

newtype CrumbTag = CrumbTag {_crumbTag :: Text}

crumbEval :: CrumbTag
crumbEval = CrumbTag "Evaluating itself"

crumbDecodeThird :: CrumbTag
crumbDecodeThird = CrumbTag "Decoding third argument"

crumbDecodeSecond :: CrumbTag
crumbDecodeSecond = CrumbTag "Decoding second argument"

crumbDecodeFirst :: CrumbTag
crumbDecodeFirst = CrumbTag "Decoding first argument"

crumbEvalFirst :: CrumbTag
crumbEvalFirst = CrumbTag "Evaluating first argument"

crumbTrueBranch :: CrumbTag
crumbTrueBranch = CrumbTag "Evaluating true branch"

crumbFalseBranch :: CrumbTag
crumbFalseBranch = CrumbTag "Evaluating false branch"

crumbEvalSecond :: CrumbTag
crumbEvalSecond = CrumbTag "Evaluating second argument"

data CrumbAutoCons = CrumbAutoCons
  { _crumbAutoConsTag :: CrumbTag,
    _crumbAutoConsLoc :: Maybe Interval
  }

data CrumbOperator = CrumbOperator
  { _crumbOperatorOp :: NockOp,
    _crumbOperatorTag :: CrumbTag,
    _crumbOperatorLoc :: Maybe Interval
  }

makeLenses ''EvalCtx

withCrumb :: (Members '[Reader EvalCtx] r) => EvalCrumb -> Sem r a -> Sem r a
withCrumb c = local (over evalStack (c :))

instance PrettyCode CrumbTag where
  ppCode (CrumbTag a) =
    return
      . annotate AnnImportant
      $ pretty a

instance PrettyCode CrumbStdlibCallArgs where
  ppCode CrumbStdlibCallArgs {..} = do
    op <- annotate AnnImportant <$> ppCode _crumbStdlibCallArgsFunction
    return ("Evaluating address to arguments to stdlib call for" <+> op)

ppCtx :: (Member (Reader Options) r) => EvalCtx -> Sem r (Doc Ann)
ppCtx c = do
  ctx <- ppCode c
  let title = annotate AnnImportant "Evaluation trace:"
  return (title <> line <> ctx <> line)

ppLoc :: (Member (Reader Options) r) => Maybe Interval -> Sem r (Doc Ann)
ppLoc = \case
  Nothing -> return mempty
  Just x -> do
    x' <- ppCode x
    return (x' <> ":")

instance PrettyCode CrumbOperator where
  ppCode CrumbOperator {..} = do
    tag <- ppCode _crumbOperatorTag
    loc <- ppLoc _crumbOperatorLoc
    op <- ppCode _crumbOperatorOp
    return (loc <+> tag <+> "for" <+> op)

instance PrettyCode CrumbAutoCons where
  ppCode CrumbAutoCons {..} = do
    let au = annotate AnnImportant "AutoCons"
    loc <- ppLoc _crumbAutoConsLoc
    tag <- ppCode _crumbAutoConsTag
    return (loc <+> tag <+> "for" <+> au)

instance PrettyCode EvalCrumb where
  ppCode = \case
    EvalCrumbAutoCons a -> ppCode a
    EvalCrumbStdlibCallArgs a -> ppCode a
    EvalCrumbOperator a -> ppCode a

instance PrettyCode EvalCtx where
  ppCode (EvalCtx l) =
    vsep <$> mapM (fmap nest' . ppCode) (reverse l)
