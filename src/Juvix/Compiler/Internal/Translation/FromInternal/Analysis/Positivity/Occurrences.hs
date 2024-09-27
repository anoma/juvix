module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Occurrences
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.ConstructorArg.Base,
    Occurrences (..),
    mkOccurrences,
    occurrencesPolarity,
    occurrencesTree,
    occurrencesAggregatePolarities,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.ConstructorArg.Base
import Juvix.Prelude

data FunctionSide
  = FunctionLeft
  | FunctionRight

instance Semigroup FunctionSide where
  a <> b = case (a, b) of
    (FunctionLeft, _) -> FunctionLeft
    (_, FunctionLeft) -> FunctionLeft
    (FunctionRight, FunctionRight) -> FunctionRight

instance Monoid FunctionSide where
  mempty = FunctionRight

data Occurrences = Occurrences
  { _occurrencesPolarity :: HashMap InductiveParam Polarity,
    _occurrencesTree :: HashMap AppLhs [Occurrences]
  }
  deriving stock (Show)

makeLenses ''Occurrences

emptyOccurrences :: Occurrences
emptyOccurrences =
  Occurrences
    { _occurrencesPolarity = mempty,
      _occurrencesTree = mempty
    }

occurrencesAggregatePolarities :: Occurrences -> HashMap InductiveParam Polarity
occurrencesAggregatePolarities = run . execState mempty . go
  where
    go :: Occurrences -> Sem '[State (HashMap InductiveParam Polarity)] ()
    go o = do
      modify (HashMap.unionWith (<>) (o ^. occurrencesPolarity))
      mapM_ go (concat (toList (o ^. occurrencesTree)))

mkOccurrences :: [ConstructorArg] -> Occurrences
mkOccurrences =
  run
    . runReader FunctionRight
    . execState emptyOccurrences
    . mapM_ addArg
  where
    getPolarity :: forall r'. (Members '[Reader FunctionSide] r') => Sem r' Polarity
    getPolarity =
      ask <&> \case
        FunctionLeft -> PolarityNegative
        FunctionRight -> PolarityStrictlyPositive

    addArg :: forall r'. (Members '[Reader FunctionSide, State Occurrences] r') => ConstructorArg -> Sem r' ()
    addArg = \case
      ConstructorArgFun fun -> goFun fun
      ConstructorArgApp a -> goApp a
      where
        registerOccurrence :: InductiveParam -> Sem r' ()
        registerOccurrence v = do
          pol <- getPolarity
          modify (over (occurrencesPolarity . at v) (Just . maybe pol (<> pol)))

        goApp :: App -> Sem r' ()
        goApp (App lhs args) = case lhs of
          AppVar v -> goVar v
          AppAxiom {} -> goArgs
          AppInductive {} -> goArgs
          where
            goVar :: InductiveParam -> Sem r' ()
            goVar v = do
              registerOccurrence v
              goArgs

            goArgs :: Sem r' ()
            goArgs = do
              let numArgs = length args
                  iniOccs = replicate numArgs emptyOccurrences
              occs :: [Occurrences] <- fromMaybe iniOccs <$> gets (^. occurrencesTree . at lhs)
              st :: Occurrences <- get
              occs' :: [Occurrences] <- for (zipExact occs args) $ \(occ, arg) -> do
                put occ
                addArg arg
                get
              put (set (occurrencesTree . at lhs) (Just occs') st)

        goFun :: Fun -> Sem r' ()
        goFun (Fun funl funr) = do
          onSide FunctionLeft (addArg funl)
          onSide FunctionRight (addArg funr)
          where
            onSide :: FunctionSide -> Sem r' () -> Sem r' ()
            onSide side = local (side <>)
