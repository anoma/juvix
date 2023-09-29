module Juvix.Compiler.Internal.Translation.FromConcrete.NamedArguments
  ( runNamedArguments,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Juvix.Compiler.Concrete.Gen qualified as Gen
import Juvix.Compiler.Concrete.Keywords
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Prelude

data BuilderState = BuilderState
  { _stateRemainingArgs :: [ArgumentBlock 'Scoped],
    _stateRemainingNames :: [NameBlock 'Scoped]
  }

makeLenses ''BuilderState

runNamedArguments ::
  forall r.
  (Members '[NameIdGen, Error ScoperError] r) =>
  NamedApplication 'Scoped ->
  Sem r Expression
runNamedArguments napp = do
  args <-
    execOutputList
      . mapError ErrNamedArgumentsError
      . execState iniBuilderState
      $ helper (getLoc napp)
  return (foldl' mkApp (ExpressionIdentifier (napp ^. namedAppName)) args)
  where
    sig :: NameSignature 'Scoped = napp ^. namedAppSignature . unIrrelevant
    mkApp :: Expression -> Expression -> Expression
    mkApp a = ExpressionApplication . Application a
    iniBuilderState :: BuilderState
    iniBuilderState =
      BuilderState
        { _stateRemainingArgs = toList (napp ^. namedAppArgs),
          _stateRemainingNames = sig ^. nameSignatureArgs
        }

helper ::
  forall r.
  (Members '[State BuilderState, Output Expression, NameIdGen, Error NamedArgumentsError] r) =>
  Interval ->
  Sem r ()
helper loc = do
  whenJustM nextArgumentGroup $ \(impl, args, isLastBlock) -> do
    checkRepeated args
    (mdefault1, names) <- nextNameGroup impl
    (pendingArgs, (omittedNames, argmap)) <- scanGroup impl names args
    emitArgs mdefault1 impl isLastBlock omittedNames argmap
    whenJust (nonEmpty pendingArgs) $ \pendingArgs' -> do
      (mdefault, sig) <- nextNameGroup Implicit
      emitImplicit mdefault False sig mempty
      moreNames <- not . null <$> gets (^. stateRemainingNames)
      if
          | moreNames -> modify' (over stateRemainingArgs (ArgumentBlock (Irrelevant Nothing) Explicit (nonEmpty' pendingArgs) :))
          | otherwise -> throw . ErrUnexpectedArguments $ UnexpectedArguments pendingArgs'
    helper loc
  where
    nextNameGroup :: IsImplicit -> Sem r (Maybe Expression, (HashMap Symbol Int))
    nextNameGroup impl = do
      remb <- gets (^. stateRemainingNames)
      case remb of
        [] -> return (Nothing, mempty)
        b : bs -> do
          let implNames = b ^. nameImplicit
              defVal = b ^. nameDefault

          modify' (set stateRemainingNames bs)
          let r = (^. nameItemIndex) <$> b ^. nameBlock
              matches = return (defVal, r)
              fails :: Sem r (Maybe Expression, (HashMap Symbol Int)) = return (Nothing, mempty)
          case (impl, implNames) of
            (Explicit, Explicit) -> matches
            (Implicit, Implicit) -> matches
            (ImplicitInstance, ImplicitInstance) -> matches
            (Explicit, Implicit) -> do
              emitImplicit defVal False r mempty
              nextNameGroup impl
            (Explicit, ImplicitInstance) -> do
              emitImplicitInstance False r mempty
              nextNameGroup impl
            (Implicit, ImplicitInstance) -> do
              emitImplicitInstance False r mempty
              nextNameGroup impl
            (ImplicitInstance, Implicit) -> do
              emitImplicit defVal False r mempty
              nextNameGroup impl
            (Implicit, Explicit) -> fails
            (ImplicitInstance, Explicit) -> fails

    nextArgumentGroup :: Sem r (Maybe (IsImplicit, [NamedArgument 'Scoped], Bool))
    nextArgumentGroup = do
      remb <- gets (^. stateRemainingArgs)
      case remb of
        [] -> return Nothing
        b : bs -> do
          let impl = b ^. argBlockImplicit
              (c, rem') = span ((== impl) . (^. argBlockImplicit)) bs
              isLastBlock = null rem'
          modify' (set stateRemainingArgs rem')
          return (Just (impl, concatMap (toList . (^. argBlockArgs)) (b : c), isLastBlock))

    checkRepeated :: [NamedArgument 'Scoped] -> Sem r ()
    checkRepeated args = whenJust (nonEmpty (findRepeated (map (^. namedArgName) args))) $ \reps ->
      throw . ErrDuplicateArgument $ DuplicateArgument reps

    emitArgs :: Maybe Expression -> IsImplicit -> Bool -> HashMap Symbol Int -> IntMap Expression -> Sem r ()
    emitArgs mdefault = \case
      Implicit -> emitImplicit mdefault
      Explicit -> emitExplicit
      ImplicitInstance -> emitImplicitInstance

    -- omitting arguments is only allowed at the end
    emitExplicit :: Bool -> HashMap Symbol Int -> IntMap Expression -> Sem r ()
    emitExplicit lastBlock omittedArgs args = do
      if
          | lastBlock ->
              unless
                (IntMap.keys args == [0 .. IntMap.size args - 1])
                (missingErr (nonEmpty' (map fst (filterMissing (HashMap.toList omittedArgs)))))
          | otherwise -> whenJust (nonEmpty (HashMap.keys omittedArgs)) missingErr
      forM_ args output
      where
        filterMissing :: [(Symbol, Int)] -> [(Symbol, Int)]
        filterMissing = case maximumGiven of
          Nothing -> id
          Just m -> filter ((< m) . snd)
        maximumGiven :: Maybe Int
        maximumGiven = fst <$> IntMap.lookupMax args
        missingErr :: NonEmpty Symbol -> Sem r ()
        missingErr = throw . ErrMissingArguments . MissingArguments loc

    emitImplicitHelper ::
      Maybe Expression ->
      (WithLoc Expression -> Expression) ->
      (HoleType 'Scoped -> Expression) ->
      Bool ->
      HashMap Symbol Int ->
      IntMap Expression ->
      Sem r ()
    emitImplicitHelper mdefault exprBraces exprHole lastBlock omittedArgs args = go 0 (IntMap.toAscList args)
      where
        go :: Int -> [(Int, Expression)] -> Sem r ()
        go n = \case
          []
            | lastBlock -> return ()
            | otherwise -> whenJust maxIx (fillUntil . succ)
          (n', e) : rest -> do
            fillUntil n'
            output (exprBraces (WithLoc (getLoc e) e))
            go (n' + 1) rest
          where
            fillUntil n' = replicateM_ (n' - n) (mkWildcard >>= output)
            mkWildcard :: (Members '[NameIdGen] r') => Sem r' Expression
            mkWildcard = fmap (exprBraces . WithLoc loc) $ case mdefault of
              Nothing -> exprHole . mkHole loc <$> freshNameId
              -- TODO shift binders in defaultVal
              Just defaultVal -> return defaultVal
        maxIx :: Maybe Int
        maxIx = fmap maximum1 . nonEmpty . toList $ omittedArgs

    emitImplicit :: Maybe Expression -> Bool -> HashMap Symbol Int -> IntMap Expression -> Sem r ()
    emitImplicit def = emitImplicitHelper def ExpressionBraces ExpressionHole

    emitImplicitInstance :: Bool -> HashMap Symbol Int -> IntMap Expression -> Sem r ()
    emitImplicitInstance = emitImplicitHelper Nothing mkDoubleBraces ExpressionInstanceHole
      where
        mkDoubleBraces :: WithLoc Expression -> Expression
        mkDoubleBraces (WithLoc eloc e) = run . runReader eloc $ do
          l <- Gen.kw delimDoubleBraceL
          r <- Gen.kw delimDoubleBraceR
          return $
            ExpressionDoubleBraces
              DoubleBracesExpression
                { _doubleBracesExpression = e,
                  _doubleBracesDelims = Irrelevant (l, r)
                }

    scanGroup ::
      IsImplicit ->
      HashMap Symbol Int ->
      [NamedArgument 'Scoped] ->
      Sem r ([NamedArgument 'Scoped], (HashMap Symbol Int, IntMap Expression))
    scanGroup impl names = runOutputList . runState names . execState mempty . mapM_ go
      where
        go ::
          (Members '[State (IntMap Expression), State (HashMap Symbol Int), State BuilderState, Output (NamedArgument 'Scoped), Error NamedArgumentsError] r') =>
          NamedArgument 'Scoped ->
          Sem r' ()
        go arg = do
          let sym = arg ^. namedArgName
          midx :: Maybe Int <- gets @(HashMap Symbol Int) (^. at sym)
          case midx of
            Just idx -> do
              modify' (IntMap.insert idx (arg ^. namedArgValue))
              modify' @(HashMap Symbol Int) (HashMap.delete sym)
            Nothing -> case impl of
              Explicit -> do
                -- the arg may belong to the next explicit group
                output arg
              Implicit ->
                throw $
                  ErrUnexpectedArguments $
                    UnexpectedArguments
                      { _unexpectedArguments = pure arg
                      }
              ImplicitInstance ->
                throw $
                  ErrUnexpectedArguments $
                    UnexpectedArguments
                      { _unexpectedArguments = pure arg
                      }
