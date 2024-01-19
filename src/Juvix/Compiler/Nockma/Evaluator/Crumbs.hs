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

data ArgName
  = Itself
  | FirstArg
  | SecondArg
  | ThirdArg
  | TrueBranch
  | FalseBranch

data CrumbAutoCons = CrumbAutoCons
  { _crumbAutoConsArgName :: ArgName,
    _crumbAutoConsLoc :: Maybe Interval
  }

data CrumbOperator = CrumbOperator
  { _crumbOperatorOp :: NockOp,
    _crumbOperatorArgName :: ArgName,
    _crumbOperatorLoc :: Maybe Interval
  }

makeLenses ''EvalCtx

withCrumb :: (Members '[Reader EvalCtx] r) => EvalCrumb -> Sem r a -> Sem r a
withCrumb c = local (over evalStack (c :))

instance PrettyCode EvalCtx where
  ppCode (EvalCtx l) = undefined
