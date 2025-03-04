module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Checker
  ( checkPositivity,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Extra.DependencyBuilder
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.ConstructorArg
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Occurrences
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.ResultBuilder
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Prelude hiding (fromEither)

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

makeLenses ''Blocking
makeLenses ''Builder

emptyBuilder :: Builder
emptyBuilder =
  Builder
    { _builderPolarities = mempty,
      _builderBlocking = mempty
    }

functionSidePolarity :: FunctionSide -> Polarity
functionSidePolarity = \case
  FunctionLeft -> PolarityNonStrictlyPositive
  FunctionRight -> PolarityStrictlyPositive

mkPositivityMutualBlocks :: [MutualBlock] -> [MutualBlock]
mkPositivityMutualBlocks m =
  run
    . runReader depInfo
    $ buildMutualBlocks ss
  where
    ss = flattenMutualBlocks m
    depInfo :: NameDependencyInfo = positivityNameDependencyInfo ss

checkPositivity ::
  forall r.
  ( Members
      '[ Reader InfoTable,
         Error TypeCheckerError,
         ResultBuilder
       ]
      r
  ) =>
  Bool ->
  [MutualBlock] ->
  Sem r ()
checkPositivity noPositivityFlag mut = do
  let mut' = mkPositivityMutualBlocks mut
  forM_ mut' (checkMutualBlockPositivity noPositivityFlag)

checkMutualBlockPositivity ::
  forall r.
  ( Members
      '[ Reader InfoTable,
         Error TypeCheckerError,
         ResultBuilder
       ]
      r
  ) =>
  Bool ->
  MutualBlock ->
  Sem r ()
checkMutualBlockPositivity noPositivityFlag mut = do
  let ldefs :: [InductiveDef] =
        mut
          ^.. mutualStatements
            . each
            . _StatementInductive

  whenJust (nonEmpty ldefs) $ \defs -> do
    args :: [ConstructorArg] <-
      concatMapM
        mkConstructorArg
        ( defs
            ^.. each
              . inductiveConstructors
              . each
              . inductiveConstructorNormalizedType
              . to fromJust
        )
    poltab <- (^. typeCheckingTablesPolarityTable) <$> getCombinedTables
    let occ :: Occurrences = mkOccurrences args
        inferredPolarities = computePolarities poltab defs occ
    forM_ defs $ \def -> do
      let params :: [InductiveParam] = def ^.. inductiveParameters . each . inductiveParamName
          getPol p = fromMaybe PolarityUnused (inferredPolarities ^. at p)
          polarities :: [Polarity] = map getPol params
          defName = def ^. inductiveName
      addPolarities (defName ^. nameId) polarities
    poltab' <- (^. typeCheckingTablesPolarityTable) <$> getCombinedTables
    let names :: NonEmpty InductiveName = (^. inductiveName) <$> defs
    unless noPositivityFlag $ do
      let neg = checkStrictlyPositive poltab' names occ
          markedPositive d = fromJust (find ((== d) . (^. inductiveName)) defs) ^. inductivePositive
      whenJust (nonEmpty (filter (not . markedPositive) neg)) $ \negTys ->
        throw $
          ErrNonStrictlyPositive
            NonStrictlyPositive
              { _nonStrictlyPositiveOccurrences = negTys
              }

-- NOTE we conservatively assume that axioms have all variables in negative positions
axiomPolarity :: Polarity
axiomPolarity = PolarityNonStrictlyPositive

-- | Returns the list of non-strictly positive types
checkStrictlyPositive ::
  PolarityTable ->
  NonEmpty InductiveName ->
  Occurrences ->
  [InductiveName]
checkStrictlyPositive tbl mutual =
  run
    . execOutputList
    . runReader PolarityStrictlyPositive
    . go
  where
    go :: forall r'. (Members '[Output InductiveName, Reader Polarity] r') => Occurrences -> Sem r' ()
    go occ = forM_ (HashMap.toList (occ ^. occurrences)) (uncurry goApp)

    mutualSet :: HashSet InductiveName
    mutualSet = hashSet mutual

    isMutual :: InductiveName -> Bool
    isMutual d = HashSet.member d mutualSet

    goArg ::
      forall r'.
      (Members '[Output InductiveName, Reader Polarity] r') =>
      Polarity ->
      Occurrences ->
      Sem r' ()
    goArg p occ = localPolarity p (go occ)

    goApp ::
      forall r'.
      (Members '[Output InductiveName, Reader Polarity] r') =>
      (FunctionSide, AppLhs) ->
      [Occurrences] ->
      Sem r' ()
    goApp (side, lhs) occ = local (functionSidePolarity side <>) $ case lhs of
      AppVar {} -> local (const PolarityNonStrictlyPositive) (mapM_ go occ)
      AppAxiom {} -> local (<> axiomPolarity) (mapM_ go occ)
      AppInductive d -> do
        ctx <- ask
        let pols = getPolarities d
        case ctx of
          PolarityUnused -> impossible
          PolarityStrictlyPositive -> return ()
          PolarityNonStrictlyPositive -> when (isMutual d) (output d)
        mapM_ (uncurry goArg) (zip pols occ)
      where
        getPolarities :: InductiveName -> [Polarity]
        getPolarities n = fromMaybe err (tbl ^. polarityTable . at (n ^. nameId))
          where
            err :: a
            err = impossibleError ("Didn't find polarities for inductive " <> ppTrace n)

localPolarity :: (Members '[Reader Polarity] r) => Polarity -> Sem r () -> Sem r ()
localPolarity p = case p of
  PolarityUnused -> const (return ())
  PolarityNonStrictlyPositive -> local (p <>)
  PolarityStrictlyPositive -> local (p <>)

computePolarities ::
  PolarityTable ->
  NonEmpty InductiveDef ->
  Occurrences ->
  HashMap InductiveParam Polarity
computePolarities tab defs topOccurrences =
  (^. builderPolarities)
    . run
    . runReader PolarityStrictlyPositive
    . execState emptyBuilder
    $ go topOccurrences
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
      sequence_
        [ addPolarity param (functionSidePolarity side)
          | ((side, AppVar param), _) <- HashMap.toList (o ^. occurrences)
        ]
      forM_ (HashMap.toList (o ^. occurrences)) (uncurry goApp)
      where
        addPolarity :: InductiveParam -> Polarity -> Sem r ()
        addPolarity var p = do
          newPol <- (p <>) <$> ask
          modify (over (builderPolarities . at var) (Just . maybe newPol (newPol <>)))
          unblock var newPol

    unblock :: forall r. (Members '[State Builder] r) => InductiveParam -> Polarity -> Sem r ()
    unblock p newPol = unblockGo
      where
        unblockGo :: Sem r ()
        unblockGo = do
          b <- gets (^. builderBlocking)
          whenJust (findAndRemove isTriggered b) $ \(triggered, rest) -> do
            modify (set builderBlocking (maybeToList (increaseMinimum triggered) ++ rest))
            runBlocking triggered
            unblockGo

        increaseMinimum :: Blocking -> Maybe Blocking
        increaseMinimum = case newPol of
          PolarityNonStrictlyPositive -> const Nothing
          PolarityStrictlyPositive -> Just . set blockingUnblockerMinimum PolarityNonStrictlyPositive
          PolarityUnused -> impossible

        isTriggered :: Blocking -> Bool
        isTriggered b = (b ^. blockingUnblockerMinimum <= newPol) && (b ^. blockingUnblocker == p)

        runBlocking :: Blocking -> Sem r ()
        runBlocking b = runReader (b ^. blockingContext) (go (b ^. blockingOccurrences))

    goApp ::
      forall r.
      (Members '[State Builder, Reader Polarity] r) =>
      (FunctionSide, AppLhs) ->
      [Occurrences] ->
      Sem r ()
    goApp (side, lhs) os = local (functionSidePolarity side <>) $ case lhs of
      AppVar {} -> goVarArgs os
      AppAxiom {} -> goAxiomArgs os
      AppInductive a -> goInductive a os

    goAxiomArgs :: (Members '[State Builder, Reader Polarity] r) => [Occurrences] -> Sem r ()
    goAxiomArgs os = local (<> axiomPolarity) (mapM_ go os)

    goVarArgs :: (Members '[State Builder, Reader Polarity] r) => [Occurrences] -> Sem r ()
    goVarArgs os = local (const PolarityNonStrictlyPositive) (mapM_ go os)

    block ::
      (Members '[State Builder, Reader Polarity] r) =>
      Polarity ->
      InductiveParam ->
      Occurrences ->
      Sem r ()
    block minPol param o = do
      ctx <- ask
      let b =
            Blocking
              { _blockingContext = ctx,
                _blockingUnblocker = param,
                _blockingOccurrences = o,
                _blockingUnblockerMinimum = minPol
              }
      modify (over builderBlocking (b :))

    goInductive ::
      (Members '[State Builder, Reader Polarity] r) =>
      InductiveName ->
      [Occurrences] ->
      Sem r ()
    goInductive d os = do
      case tab ^. polarityTable . at (d ^. nameId) of
        Just (pols :: [Polarity]) ->
          forM_ (zip pols os) $ \(pol :: Polarity, o :: Occurrences) -> do
            localPolarity pol (go o)
        Nothing -> do
          pols :: [(InductiveParam, Maybe Polarity)] <- getInductivePolarities d
          forM_ (zip pols os) $ \((p :: InductiveParam, mpol :: Maybe Polarity), o :: Occurrences) ->
            case mpol of
              Nothing -> block PolarityStrictlyPositive p o
              Just pol -> case pol of
                PolarityUnused -> impossible
                PolarityStrictlyPositive -> do
                  block PolarityNonStrictlyPositive p o
                  localPolarity PolarityStrictlyPositive (go o)
                PolarityNonStrictlyPositive -> localPolarity PolarityNonStrictlyPositive (go o)
