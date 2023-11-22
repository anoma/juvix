{-# LANGUAGE FunctionalDependencies #-}

module Juvix.Compiler.Concrete.Data.NameSignature.Builder
  ( mkNameSignature,
    mkRecordNameSignature,
    HasNameSignature,
    -- to supress unused warning
    getBuilder,
  )
where

import Juvix.Compiler.Concrete.Data.NameSignature.Error
import Juvix.Compiler.Concrete.Extra (symbolParsed)
import Juvix.Compiler.Concrete.Gen qualified as Gen
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Prelude

data NameSignatureBuilder s m a where
  AddSymbol :: IsImplicit -> Maybe (ArgDefault s) -> SymbolType s -> ExpressionType s -> NameSignatureBuilder s m ()
  EndBuild :: Proxy s -> NameSignatureBuilder s m a
  -- | for debugging
  GetBuilder :: NameSignatureBuilder s m (BuilderState s)

data BuilderState (s :: Stage) = BuilderState
  { _stateCurrentImplicit :: Maybe IsImplicit,
    _stateNextIx :: Int,
    -- | maps to itself
    _stateSymbols :: HashMap Symbol (SymbolType s),
    _stateReverseClosedBlocks :: [NameBlock s],
    _stateCurrentBlock :: HashMap Symbol (NameItem s)
  }

makeLenses ''BuilderState
makeSem ''NameSignatureBuilder

class HasNameSignature (s :: Stage) d | d -> s where
  addArgs :: (Members '[NameSignatureBuilder s] r) => d -> Sem r ()

instance (SingI s) => HasNameSignature s (AxiomDef s) where
  addArgs :: (Members '[NameSignatureBuilder s] r) => AxiomDef s -> Sem r ()
  addArgs a = addExpressionType (a ^. axiomType)

instance (SingI s) => HasNameSignature s (FunctionDef s) where
  addArgs a = do
    mapM_ addSigArg (a ^. signArgs)
    whenJust (a ^. signRetType) addExpressionType

instance (SingI s) => HasNameSignature s ([InductiveParameters s], ConstructorDef s) where
  addArgs ::
    forall r.
    (Members '[NameSignatureBuilder s] r) =>
    ([InductiveParameters s], ConstructorDef s) ->
    Sem r ()
  addArgs (iparams, c) = do
    mapM_ addConstructorParams iparams
    addRhs (c ^. constructorRhs)
    where
      addRecord :: RhsRecord s -> Sem r ()
      addRecord RhsRecord {..} = mapM_ addField _rhsRecordStatements
        where
          addField :: RecordStatement s -> Sem r ()
          addField = \case
            RecordStatementField RecordField {..} -> addSymbol @s Explicit Nothing _fieldName _fieldType
            RecordStatementOperator {} -> return ()

      addRhs :: ConstructorRhs s -> Sem r ()
      addRhs = \case
        ConstructorRhsGadt g -> addExpressionType (g ^. rhsGadtType)
        ConstructorRhsRecord g -> addRecord g
        ConstructorRhsAdt {} -> return ()

instance (SingI s) => HasNameSignature s (InductiveDef s) where
  addArgs a = do
    mapM_ addInductiveParams (a ^. inductiveParameters)
    whenJust (a ^. inductiveType) addExpressionType

mkNameSignature ::
  forall s d r.
  (SingI s, Members '[Error ScoperError] r, HasNameSignature s d) =>
  d ->
  Sem r (NameSignature s)
mkNameSignature d = do
  fmap (fromBuilderState . fromLeft impossible)
    . mapError ErrNameSignature
    . runError @(BuilderState s)
    . evalState iniBuilderState
    . re
    $ do
      addArgs d
      endBuild (Proxy @s)

iniBuilderState :: BuilderState s
iniBuilderState =
  BuilderState
    { _stateCurrentImplicit = Nothing,
      _stateNextIx = 0,
      _stateSymbols = mempty,
      _stateReverseClosedBlocks = [],
      _stateCurrentBlock = mempty
    }

fromBuilderState :: forall s. BuilderState s -> NameSignature s
fromBuilderState b =
  NameSignature
    { _nameSignatureArgs = reverse (addCurrent (b ^. stateReverseClosedBlocks))
    }
  where
    addCurrent :: [NameBlock s] -> [NameBlock s]
    addCurrent
      | null (b ^. stateCurrentBlock) = id
      | Just i <- b ^. stateCurrentImplicit =
          (NameBlock (b ^. stateCurrentBlock) i :)
      | otherwise = id

addExpression :: forall r. (Members '[NameSignatureBuilder 'Scoped] r) => Expression -> Sem r ()
addExpression = \case
  ExpressionFunction f -> addFunction f
  _ -> endBuild (Proxy @'Scoped)
  where
    addFunction :: Function 'Scoped -> Sem r ()
    addFunction f = do
      addFunctionParameters (f ^. funParameters)
      addExpression (f ^. funReturn)

addFunctionParameters :: forall s r. (Members '[NameSignatureBuilder s] r) => FunctionParameters s -> Sem r ()
addFunctionParameters FunctionParameters {..} = forM_ _paramNames addParameter
  where
    addParameter :: FunctionParameter s -> Sem r ()
    addParameter = \case
      FunctionParameterName p -> addSymbol @s _paramImplicit Nothing p _paramType
      FunctionParameterWildcard {} -> endBuild (Proxy @s)

addExpressionType :: forall s r. (SingI s, Members '[NameSignatureBuilder s] r) => ExpressionType s -> Sem r ()
addExpressionType = case sing :: SStage s of
  SParsed -> addAtoms
  SScoped -> addExpression

addAtoms :: forall r. (Members '[NameSignatureBuilder 'Parsed] r) => ExpressionAtoms 'Parsed -> Sem r ()
addAtoms atoms = addAtom . (^. expressionAtoms . _head1) $ atoms
  where
    addAtom :: ExpressionAtom 'Parsed -> Sem r ()
    addAtom = \case
      AtomFunction f -> do
        addFunctionParameters (f ^. funParameters)
        addAtoms (f ^. funReturn)
      _ -> endBuild (Proxy @'Parsed)

addInductiveParams' :: forall s r. (SingI s, Members '[NameSignatureBuilder s] r) => IsImplicit -> InductiveParameters s -> Sem r ()
addInductiveParams' i a =
  forM_ (a ^. inductiveParametersNames) $ \sym ->
    addSymbol @s i Nothing sym ty
  where
    mty = a ^? inductiveParametersRhs . _Just . inductiveParametersType
    ty = fromMaybe defaultType mty
    defaultType = run (runReader (getLoc a) Gen.smallUniverseExpression)

addInductiveParams :: (SingI s, Members '[NameSignatureBuilder s] r) => InductiveParameters s -> Sem r ()
addInductiveParams = addInductiveParams' Explicit

addConstructorParams :: (SingI s, Members '[NameSignatureBuilder s] r) => InductiveParameters s -> Sem r ()
addConstructorParams = addInductiveParams' Implicit

addSigArg :: (SingI s, Members '[NameSignatureBuilder s] r) => SigArg s -> Sem r ()
addSigArg a = forM_ (a ^. sigArgNames) $ \case
  ArgumentSymbol s ->
    addSymbol
      (a ^. sigArgImplicit)
      (a ^. sigArgDefault)
      s
      (fromMaybe defaultType (a ^. sigArgType))
    where
      defaultType = run (runReader (getLoc a) Gen.smallUniverseExpression)
  ArgumentWildcard {} -> return ()

type Re s r = State (BuilderState s) ': Error (BuilderState s) ': Error NameSignatureError ': r

re ::
  forall s r a.
  (SingI s) =>
  Sem (NameSignatureBuilder s ': r) a ->
  Sem (Re s r) a
re = reinterpret3 $ \case
  AddSymbol impl mdef k ty -> addSymbol' impl mdef k ty
  EndBuild {} -> endBuild'
  GetBuilder -> get
{-# INLINE re #-}

addSymbol' :: forall s r. (SingI s) => IsImplicit -> Maybe (ArgDefault s) -> SymbolType s -> ExpressionType s -> Sem (Re s r) ()
addSymbol' impl mdef sym ty = do
  curImpl <- gets @(BuilderState s) (^. stateCurrentImplicit)
  if
      | Just impl == curImpl -> addToCurrentBlock
      | otherwise -> startNewBlock
  where
    errDuplicateName :: Symbol -> Sem (Re s r) ()
    errDuplicateName _dupNameFirst =
      throw $
        ErrDuplicateName
          DuplicateName
            { _dupNameSecond = symbolParsed sym,
              ..
            }

    addToCurrentBlock :: Sem (Re s r) ()
    addToCurrentBlock = do
      idx <- gets @(BuilderState s) (^. stateNextIx)
      let itm =
            NameItem
              { _nameItemDefault = mdef,
                _nameItemSymbol = sym,
                _nameItemIndex = idx,
                _nameItemType = ty
              }
          psym = symbolParsed sym
      modify' @(BuilderState s) (over stateNextIx succ)
      whenJustM (gets @(BuilderState s) (^. stateSymbols . at psym)) (errDuplicateName . symbolParsed)
      modify' @(BuilderState s) (set (stateSymbols . at psym) (Just sym))
      modify' @(BuilderState s) (set (stateCurrentBlock . at psym) (Just itm))

    startNewBlock :: Sem (Re s r) ()
    startNewBlock = do
      curBlock <- gets @(BuilderState s) (^. stateCurrentBlock)
      mcurImpl <- gets @(BuilderState s) (^. stateCurrentImplicit)
      modify' @(BuilderState s) (set stateCurrentImplicit (Just impl))
      modify' @(BuilderState s) (set stateCurrentBlock mempty)
      modify' @(BuilderState s) (set stateNextIx 0)
      whenJust mcurImpl $ \curImpl ->
        modify' (over stateReverseClosedBlocks (NameBlock curBlock curImpl :))
      addSymbol' impl mdef sym ty

endBuild' :: forall s r a. Sem (Re s r) a
endBuild' = get @(BuilderState s) >>= throw

mkRecordNameSignature :: forall s. (SingI s) => [InductiveParameters s] -> RhsRecord s -> RecordNameSignature s
mkRecordNameSignature ps rhs =
    RecordNameSignature $
      indexedByHash (symbolParsed . (^. nameItemSymbol)) (run (execOutputList (evalState 0 helper)))
  where
    helper :: forall r. (r ~ '[State Int, Output (NameItem s)]) => Sem r ()
    helper = do
      -- forM_ ps emitParameters
      forOf_ (rhsRecordStatements . each . _RecordStatementField) rhs emitField
      where
        emitItem :: (Int -> NameItem s) -> Sem r ()
        emitItem mkitem = do
          idx <- get
          output (mkitem idx)
          put (idx + 1)

        emitField :: RecordField s -> Sem r ()
        emitField field = emitItem $ \_nameItemIndex ->
          NameItem
            { _nameItemSymbol = field ^. fieldName,
              _nameItemType = field ^. fieldType,
              _nameItemDefault = Nothing,
              _nameItemIndex
            }

        -- emitParameters :: InductiveParameters s -> Sem r ()
        -- emitParameters params = forM_ (params ^. inductiveParametersNames) emitParam
        --   where
        --     -- FIXME  implicitness!!
        --     emitParam :: SymbolType s -> Sem r ()
        --     emitParam sym = emitItem $ \_nameItemIndex ->
        --       NameItem
        --         { _nameItemSymbol = sym,
        --           _nameItemType = fromMaybe defaultType (params ^? inductiveParametersRhs . _Just . inductiveParametersType),
        --           _nameItemDefault = Nothing,
        --           _nameItemIndex
        --         }
        --       where
        --         defaultType = run (runReader (getLocSymbolType sym) Gen.smallUniverseExpression)
