module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.CheckerNew where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.ConstructorArg
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Occurrences
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.ResultBuilder
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Prelude hiding (fromEither)

data CheckPositivityArgs = CheckPositivityArgs
  { _checkPositivityArgsInductive :: InductiveInfo,
    _checkPositivityArgsConstructorName :: Name,
    _checkPositivityArgsInductiveName :: Name,
    _checkPositivityArgsRecursionLimit :: Int,
    _checkPositivityArgsErrorReference :: Maybe Expression,
    _checkPositivityArgsTypeOfConstructorArg :: Expression
  }

data Builder = Builder
  { _builderPolarities :: HashMap InductiveParam Polarity,
    _builderBlocking :: [Blocking]
  }

data Blocking = Blocking
  { _blockingContext :: Polarity,
    _blockingUnblocker :: InductiveParam,
    -- | The unblocker has to have at least this polarity to unblock
    _blockingUnblockerMinimum :: Polarity,
    _blockingOccurrences :: Occurrences
  }

makeLenses ''CheckPositivityArgs
makeLenses ''Builder

emptyBuilder :: Builder
emptyBuilder =
  Builder
    { _builderPolarities = mempty,
      _builderBlocking = mempty
    }

checkPositivity ::
  forall r.
  ( Members
      '[ Reader InfoTable,
         Error TypeCheckerError,
         ResultBuilder,
         Inference
       ]
      r
  ) =>
  MutualBlock ->
  Sem r ()
checkPositivity mut = do
  let ldefs :: [InductiveDef] =
        mut
          ^.. mutualStatements
            . each
            . _StatementInductive

  whenJust (nonEmpty ldefs) $ \defs -> do
    args :: [ConstructorArg] <-
      concatMapM
        (strongNormalize >=> mkConstructorArg)
        ( defs
            ^.. each
              . inductiveConstructors
              . each
              . inductiveConstructorType
        )
    poltab <- (^. typeCheckingTablesPolarityTable) <$> getCombinedTables
    let occ :: Occurrences = mkOccurrences args
        inferredPolarities = computePolarities poltab defs occ
    forM_ defs $ \def -> do
      let params :: [InductiveParam] = def ^.. inductiveParameters . each . inductiveParamName
          getPol p = fromMaybe PolarityUnused (inferredPolarities ^. at p)
          polarities :: [Polarity] = map getPol params
          defName = def ^. inductiveName
      traceM ("add polarities " <> ppTrace defName <> ": " <> prettyText polarities)
      addPolarities (defName ^. nameId) polarities

computePolarities :: PolarityTable -> NonEmpty InductiveDef -> Occurrences -> HashMap InductiveParam Polarity
computePolarities tab defs topOccurrences =
  (^. builderPolarities)
    . run
    . runReader PolarityStrictlyPositive
    . execState emptyBuilder
    . go
    $ topOccurrences
  where
    defsByName :: HashMap InductiveName InductiveDef
    defsByName = indexedByHash (^. inductiveName) defs

    getDef :: InductiveName -> InductiveDef
    getDef d = fromJust (defsByName ^. at d)

    -- Gets the current polarities of an inductive definition in the current mutual block
    getInductivePolarities ::
      (Members '[State Builder] r) =>
      InductiveName ->
      Sem r [(InductiveParam, Maybe Polarity)]
    getInductivePolarities d = do
      b <- get
      let params = getDef d ^. inductiveParameters
      return
        [ (name, b ^. builderPolarities . at name)
          | p :: InductiveParameter <- params,
            let name = p ^. inductiveParamName
        ]

    go :: forall r. (Members '[State Builder, Reader Polarity] r) => Occurrences -> Sem r ()
    go o = do
      forM_ (HashMap.toList (o ^. occurrencesPolarity)) (uncurry addPolarity)
      forM_ (HashMap.toList (o ^. occurrencesTree)) (uncurry goApp)
      where
        addPolarity :: InductiveParam -> Polarity -> Sem r ()
        addPolarity var p = do
          modif <- ask
          b <- get
          let old :: Maybe Polarity = b ^. builderPolarities . at var
              new :: Polarity = maybe id (<>) old (p <> modif)
          modify (set (builderPolarities . at var) (Just new))
          when (old < Just new) (unblock var new)

    unblock :: InductiveParam -> Polarity -> Sem r ()
    unblock p newPol = undefined

    goApp :: forall r. (Members '[State Builder, Reader Polarity] r) => AppLhs -> [Occurrences] -> Sem r ()
    goApp lhs os = case lhs of
      AppVar {} -> goVarArgs os
      AppAxiom {} -> goAxiomArgs os
      AppInductive a -> goInductive a os

    localPolarity :: (Members '[Reader Polarity] r) => Polarity -> Sem r () -> Sem r ()
    localPolarity = \case
      PolarityUnused -> const (return ())
      PolarityNegative -> local (const PolarityNegative)
      PolarityStrictlyPositive -> local (const PolarityStrictlyPositive)

    -- NOTE we assume that axioms have all variables in strictly positive positions
    goAxiomArgs :: (Members '[State Builder, Reader Polarity] r) => [Occurrences] -> Sem r ()
    goAxiomArgs os = mapM_ go os

    goVarArgs :: (Members '[State Builder, Reader Polarity] r) => [Occurrences] -> Sem r ()
    goVarArgs os = mapM_ go os

    blockOn ::
      (Members '[State Builder, Reader Polarity] r) =>
      Polarity ->
      InductiveParam ->
      Occurrences ->
      Sem r ()
    blockOn minPol param o = do
      ctx <- ask
      let b =
            Blocking
              { _blockingContext = ctx,
                _blockingUnblocker = param,
                _blockingOccurrences = o,
                _blockingUnblockerMinimum = minPol
              }
      modify (over builderBlocking (b :))

    goInductive :: (Members '[State Builder, Reader Polarity] r) => InductiveName -> [Occurrences] -> Sem r ()
    goInductive d os =
      case tab ^. polarityTable . at (d ^. nameId) of
        Just pols ->
          forM_ (zipExact pols os) $ \(pol :: Polarity, o :: Occurrences) -> do
            localPolarity pol (go o)
        Nothing -> do
          pols :: [(InductiveParam, Maybe Polarity)] <- getInductivePolarities d
          traceM ("mutually recursive TODO" <> ppTrace d <> "\n" <> ppTrace topOccurrences <> "\n" <> ppTrace pols)
          forM_ (zipExact pols os) $ \((p :: InductiveParam, mpol :: Maybe Polarity), o :: Occurrences) -> do
            traceM ("mpol " <> ppTrace mpol)
            case mpol of
              Nothing -> blockOn PolarityStrictlyPositive p o
              Just pol -> case pol of
                PolarityNegative -> localPolarity pol (go o)
                PolarityStrictlyPositive -> do
                  blockOn (succ pol) p o
                  localPolarity pol (go o)
                PolarityUnused -> impossible
