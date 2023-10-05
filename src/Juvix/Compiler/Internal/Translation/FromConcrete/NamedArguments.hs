module Juvix.Compiler.Internal.Translation.FromConcrete.NamedArguments
  ( runNamedArguments,
    NameSignatures,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Gen qualified as Gen
import Juvix.Compiler.Concrete.Keywords
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Prelude

type NameSignatures = HashMap NameId (NameSignature 'Scoped)

data BuilderState = BuilderState
  { _stateRemainingArgs :: [ArgumentBlock 'Scoped],
    _stateRemainingNames :: [NameBlock 'Scoped]
  }

makeLenses ''BuilderState

runNamedArguments ::
  forall r.
  (Members '[NameIdGen, Error ScoperError, Reader NameSignatures, Reader NameSignatures] r) =>
  NamedApplication 'Scoped ->
  Sem r Expression
runNamedArguments napp = do
  iniSt <- mkIniBuilderState
  args <-
    execOutputList
      . mapError ErrNamedArgumentsError
      . execState iniSt
      $ helper (getLoc napp)
  return (foldl' mkApp (ExpressionIdentifier (napp ^. namedAppName)) args)
  where
    -- sig :: NameSignature 'Scoped = napp ^. namedAppSignature . unIrrelevant
    mkApp :: Expression -> Expression -> Expression
    mkApp a = ExpressionApplication . Application a

    mkIniBuilderState :: Sem r BuilderState
    mkIniBuilderState = do
      sig <- asks @NameSignatures (^?! at (napp ^. namedAppName . scopedIdenName . S.nameId) . _Just)
      return
        BuilderState
          { _stateRemainingArgs = toList (napp ^. namedAppArgs),
            _stateRemainingNames = sig ^. nameSignatureArgs
          }

type Defaults = IntMap (ArgDefault 'Scoped)

mkDefaults :: [NameItem 'Scoped] -> IntMap (ArgDefault 'Scoped)
mkDefaults l =
  IntMap.fromList
    [ (i ^. nameItemIndex, def) | i <- l, Just def <- [i ^. nameItemDefault]
    ]

helper ::
  forall r.
  (Members '[State BuilderState, Output Expression, NameIdGen, Error NamedArgumentsError] r) =>
  Interval ->
  Sem r ()
helper loc = do
  whenJustM nextArgumentGroup $ \(impl, args, isLastBlock) -> do
    checkRepeated args
    names :: [NameItem 'Scoped] <- nextNameGroup impl

    (pendingArgs, (omittedNames, argmap)) <- scanGroup impl names args
    emitArgs impl isLastBlock (mkDefaults names) omittedNames argmap
    whenJust (nonEmpty pendingArgs) $ \pendingArgs' -> do
      sig <- nextNameGroup Implicit
      emitImplicit False (mkDefaults sig) sig mempty
      moreNames <- not . null <$> gets (^. stateRemainingNames)
      if
          | moreNames -> modify' (over stateRemainingArgs (ArgumentBlock (Irrelevant Nothing) Explicit (nonEmpty' pendingArgs) :))
          | otherwise -> throw . ErrUnexpectedArguments $ UnexpectedArguments pendingArgs'
    helper loc
  where
    nextNameGroup :: IsImplicit -> Sem r [NameItem 'Scoped]
    nextNameGroup implArgs = do
      remb <- gets (^. stateRemainingNames)
      case remb of
        [] -> return mempty
        (b :: NameBlock 'Scoped) : bs -> do
          let implSig = b ^. nameImplicit
              defaults = mkDefaults (toList (b ^. nameBlock))
          modify' (set stateRemainingNames bs)
          let r = toList (b ^. nameBlock)
              matches = return r
          case (implArgs, implSig) of
            (Explicit, Explicit) -> matches
            (Implicit, Implicit) -> matches
            (ImplicitInstance, ImplicitInstance) -> matches
            (Explicit, Implicit) -> do
              emitImplicit False defaults r mempty
              nextNameGroup implArgs
            (Explicit, ImplicitInstance) -> do
              emitImplicitInstance False defaults r mempty
              nextNameGroup implArgs
            (Implicit, ImplicitInstance) -> do
              emitImplicitInstance False defaults r mempty
              nextNameGroup implArgs
            (ImplicitInstance, Implicit) -> do
              emitImplicit False defaults r mempty
              nextNameGroup implArgs
            (Implicit, Explicit) -> return mempty
            (ImplicitInstance, Explicit) -> return mempty

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

    emitArgs :: IsImplicit -> Bool -> Defaults -> [NameItem 'Scoped] -> IntMap Expression -> Sem r ()
    emitArgs = \case
      Implicit -> emitImplicit
      Explicit -> emitExplicit
      ImplicitInstance -> emitImplicitInstance
      where
        -- omitting arguments is only allowed at the end
        emitExplicit :: Bool -> Defaults -> [NameItem 'Scoped] -> IntMap Expression -> Sem r ()
        emitExplicit lastBlock _ omittedArgs args = do
          if
              | lastBlock ->
                  unless
                    (IntMap.keys args == [0 .. IntMap.size args - 1])
                    (missingErr (nonEmpty' (map (^. nameItemSymbol) (filterMissing omittedArgs))))
              | otherwise -> whenJust (nonEmpty (map (^. nameItemSymbol) omittedArgs)) missingErr
          forM_ args output
          where
            filterMissing :: [NameItem 'Scoped] -> [NameItem 'Scoped]
            filterMissing = case maximumGiven of
              Nothing -> id
              Just m -> filter ((< m) . (^. nameItemIndex))

            maximumGiven :: Maybe Int
            maximumGiven = fst <$> IntMap.lookupMax args

            missingErr :: NonEmpty Symbol -> Sem r ()
            missingErr = throw . ErrMissingArguments . MissingArguments loc

    emitImplicitHelper ::
      (WithLoc Expression -> Expression) ->
      (HoleType 'Scoped -> Expression) ->
      Bool ->
      Defaults ->
      [NameItem 'Scoped] ->
      IntMap Expression ->
      Sem r ()
    emitImplicitHelper exprBraces exprHole lastBlock defaults omittedArgs args = go 0 (IntMap.toAscList args)
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
            fillUntil n' = forM_ [n .. n' - 1] (fillPosition >=> output)

            fillPosition :: (Members '[NameIdGen] r') => Int -> Sem r' Expression
            fillPosition idx = do
              e' <-
                exprBraces . WithLoc loc
                  <$> case defaults ^. at idx of
                    Nothing -> exprHole . mkHole loc <$> freshNameId
                    -- TODO generate fresh binders
                    -- TODO update location
                    Just d -> return (d ^. argDefaultValue)
              -- traceM ("fill position " <> show idx <> " : " <> ppTrace e')
              return e'
        maxIx :: Maybe Int
        maxIx = fmap maximum1 . nonEmpty . map (^. nameItemIndex) $ omittedArgs

    emitImplicit :: Bool -> Defaults -> [NameItem 'Scoped] -> IntMap Expression -> Sem r ()
    emitImplicit = emitImplicitHelper ExpressionBraces ExpressionHole

    emitImplicitInstance :: Bool -> Defaults -> [NameItem 'Scoped] -> IntMap Expression -> Sem r ()
    emitImplicitInstance = emitImplicitHelper mkDoubleBraces ExpressionInstanceHole
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
      [NameItem 'Scoped] ->
      [NamedArgument 'Scoped] ->
      Sem r ([NamedArgument 'Scoped], ([NameItem 'Scoped], IntMap Expression))
    scanGroup impl names =
      fmap (second (first toList))
        . runOutputList
        . runState namesBySymbol
        . execState mempty
        . mapM_ go
      where
        namesBySymbol :: HashMap Symbol (NameItem 'Scoped)
        namesBySymbol = HashMap.fromList [(i ^. nameItemSymbol, i) | i <- names]
        go ::
          (Members '[State (IntMap Expression), State (HashMap Symbol (NameItem 'Scoped)), State BuilderState, Output (NamedArgument 'Scoped), Error NamedArgumentsError] r') =>
          NamedArgument 'Scoped ->
          Sem r' ()
        go arg = do
          let sym = arg ^. namedArgName
          midx :: Maybe (NameItem 'Scoped) <- gets @(HashMap Symbol (NameItem 'Scoped)) (^. at sym)
          case midx of
            Just idx -> do
              modify' (IntMap.insert (idx ^. nameItemIndex) (arg ^. namedArgValue))
              modify' @(HashMap Symbol (NameItem 'Scoped)) (HashMap.delete sym)
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
