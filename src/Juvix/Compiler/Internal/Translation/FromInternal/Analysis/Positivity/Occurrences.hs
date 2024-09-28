module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Occurrences
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.ConstructorArg.Base,
    Occurrences (..),
    FunctionSide (..),
    functionSidePolarity,
    mkOccurrences,
    occurrences,
  )
where

import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.ConstructorArg.Base
import Juvix.Prelude
import Juvix.Prelude.Pretty

data FunctionSide
  = FunctionLeft
  | FunctionRight
  deriving stock (Show, Eq, Generic)

instance Pretty FunctionSide where
  pretty = \case
    FunctionLeft -> "left"
    FunctionRight -> "right"

instance Hashable FunctionSide

instance Semigroup FunctionSide where
  a <> b = case (a, b) of
    (FunctionLeft, _) -> FunctionLeft
    (_, FunctionLeft) -> FunctionLeft
    (FunctionRight, FunctionRight) -> FunctionRight

instance Monoid FunctionSide where
  mempty = FunctionRight

newtype Occurrences = Occurrences
  { _occurrences :: HashMap (FunctionSide, AppLhs) [Occurrences]
  }
  deriving stock (Show)

makeLenses ''Occurrences

functionSidePolarity :: FunctionSide -> Polarity
functionSidePolarity = \case
  FunctionLeft -> PolarityNegative
  FunctionRight -> PolarityStrictlyPositive

emptyOccurrences :: Occurrences
emptyOccurrences =
  Occurrences
    { _occurrences = mempty
    }

mkOccurrences :: [ConstructorArg] -> Occurrences
mkOccurrences =
  run
    . runReader FunctionRight
    . execState emptyOccurrences
    . mapM_ addArg
  where
    addArg :: forall r'. (Members '[Reader FunctionSide, State Occurrences] r') => ConstructorArg -> Sem r' ()
    addArg = \case
      ConstructorArgFun fun -> goFun fun
      ConstructorArgApp a -> goApp a
      ConstructorArgType -> return ()
      where
        goApp :: App -> Sem r' ()
        goApp (App lhs args) = case lhs of
          AppVar {} -> goArgs (<> FunctionLeft)
          AppAxiom {} -> goArgs id
          AppInductive {} -> goArgs id
          where
            goArgs :: (FunctionSide -> FunctionSide) -> Sem r' ()
            goArgs recurModif = do
              side <- ask
              let numArgs = length args
                  iniOccs = replicate numArgs emptyOccurrences
                  k = (side, lhs)
              occs :: [Occurrences] <- fromMaybe iniOccs <$> gets (^. occurrences . at k)
              st :: Occurrences <- get
              occs' :: [Occurrences] <- local recurModif . for (zipExact occs args) $ \(occ, arg) -> do
                put occ
                addArg arg
                get
              put (set (occurrences . at k) (Just occs') st)

        goFun :: Fun -> Sem r' ()
        goFun (Fun funl funr) = do
          onSide FunctionLeft (addArg funl)
          onSide FunctionRight (addArg funr)
          where
            onSide :: FunctionSide -> Sem r' () -> Sem r' ()
            onSide side = local (side <>)
