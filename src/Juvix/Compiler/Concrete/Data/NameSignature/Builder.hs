{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Compiler.Concrete.Data.NameSignature.Builder
  ( mkNameSignature,
    mkRecordNameSignature,
    HasNameSignature,
    -- to suppress unused warning
    getBuilder,
  )
where

import Juvix.Compiler.Concrete.Data.NameSignature.Error
import Juvix.Compiler.Concrete.Gen qualified as Gen
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Prelude

data NameSignatureBuilder s :: Effect where
  AddArgument ::
    IsImplicit ->
    Maybe (ArgDefault s) ->
    Maybe (SymbolType s) ->
    ExpressionType s ->
    NameSignatureBuilder s m ()
  EndBuild :: Proxy s -> NameSignatureBuilder s m a
  -- | for debugging
  GetBuilder :: NameSignatureBuilder s m (BuilderState s)

data BuilderState (s :: Stage) = BuilderState
  { _stateCurrentImplicit :: Maybe IsImplicit,
    _stateNextIx :: Int,
    -- | maps to itself
    _stateSymbols :: HashMap Symbol (SymbolType s),
    _stateReverseClosedBlocks :: [NameBlock s],
    -- | Items stored in reverse order
    _stateCurrentBlockReverse :: [NameItem s]
  }

makeLenses ''BuilderState
makeSem ''NameSignatureBuilder

addSymbol ::
  (Members '[NameSignatureBuilder s] r) =>
  IsImplicit ->
  Maybe (ArgDefault s) ->
  SymbolType s ->
  ExpressionType s ->
  Sem r ()
addSymbol isImplicit mdefault sym ty = addArgument isImplicit mdefault (Just sym) ty

class HasNameSignature (s :: Stage) d | d -> s where
  addArgs :: (Members '[NameSignatureBuilder s] r) => d -> Sem r ()

instance (SingI s) => HasNameSignature s (AxiomDef s) where
  addArgs :: (Members '[NameSignatureBuilder s] r) => AxiomDef s -> Sem r ()
  addArgs a = addExpressionType (a ^. axiomType)

instance (SingI s) => HasNameSignature s (FunctionDef s) where
  addArgs a = do
    mapM_ addSigArg (a ^. signArgs)
    whenJust (a ^. signRetType) addExpressionType

instance (SingI s) => HasNameSignature s (InductiveDef s, ConstructorDef s) where
  addArgs ::
    forall r.
    (Members '[NameSignatureBuilder s] r) =>
    (InductiveDef s, ConstructorDef s) ->
    Sem r ()
  addArgs (i, c) = do
    mapM_ addConstructorParams (i ^. inductiveParameters)
    addRhs (c ^. constructorRhs)
    where
      addRecord :: RhsRecord s -> Sem r ()
      addRecord RhsRecord {..} = mapM_ addField _rhsRecordStatements
        where
          addField :: RecordStatement s -> Sem r ()
          addField = \case
            RecordStatementField RecordField {..} ->
              addSymbol @s (fromIsImplicitField _fieldIsImplicit) Nothing _fieldName _fieldType
            RecordStatementSyntax d -> goSyntax d

          goSyntax :: RecordSyntaxDef s -> Sem r ()
          goSyntax = \case
            RecordSyntaxOperator {} -> return ()
            RecordSyntaxIterator {} -> return ()

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
      _stateCurrentBlockReverse = mempty
    }

fromBuilderState :: forall s. BuilderState s -> NameSignature s
fromBuilderState b =
  NameSignature
    { _nameSignatureArgs = reverse (addCurrent (b ^. stateReverseClosedBlocks))
    }
  where
    addCurrent :: [NameBlock s] -> [NameBlock s]
    addCurrent = case (nonEmpty (reverse (b ^. stateCurrentBlockReverse)), b ^. stateCurrentImplicit) of
      (Just newBlock, Just i) -> (mkNameBlock newBlock i :)
      _ -> id

mkNameBlock :: NonEmpty (NameItem s) -> IsImplicit -> NameBlock s
mkNameBlock items impl =
  NameBlock
    { _nameBlockItems = items,
      _nameBlockImplicit = impl
    }

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

addSigArg :: forall s r. (SingI s, Members '[NameSignatureBuilder s] r) => SigArg s -> Sem r ()
addSigArg a = case a ^. sigArgNames of
  SigArgNamesInstance {} -> addArg (ArgumentWildcard (Wildcard (getLoc a)))
  SigArgNames ns -> mapM_ addArg ns
  where
    defaultType :: ExpressionType s
    defaultType = run (runReader (getLoc a) Gen.smallUniverseExpression)

    addArg :: Argument s -> Sem r ()
    addArg arg =
      let sym :: Maybe (SymbolType s) = case arg of
            ArgumentSymbol sy -> Just sy
            ArgumentWildcard {} -> Nothing
       in addArgument
            (a ^. sigArgImplicit)
            (a ^. sigArgDefault)
            sym
            (fromMaybe defaultType (a ^. sigArgType))

type Re s r = State (BuilderState s) ': Error (BuilderState s) ': Error NameSignatureError ': r

re ::
  forall s r a.
  (SingI s) =>
  Sem (NameSignatureBuilder s ': r) a ->
  Sem (Re s r) a
re = interpretTop3 $ \case
  AddArgument impl mdef k ty -> addArgument' impl mdef k ty
  EndBuild {} -> endBuild'
  GetBuilder -> get
{-# INLINE re #-}

addArgument' ::
  forall s r.
  (SingI s) =>
  IsImplicit ->
  Maybe (ArgDefault s) ->
  Maybe (SymbolType s) ->
  ExpressionType s ->
  Sem (Re s r) ()
addArgument' impl mdef msym ty = do
  curImpl <- gets @(BuilderState s) (^. stateCurrentImplicit)
  if
      | Just impl == curImpl -> addToCurrentBlock
      | otherwise -> startNewBlock
  where
    errDuplicateName :: SymbolType s -> Symbol -> Sem (Re s r) ()
    errDuplicateName sym _dupNameFirst =
      throw $
        ErrDuplicateName
          DuplicateName
            { _dupNameSecond = symbolParsed sym,
              ..
            }

    getNextIx :: (Members '[State (BuilderState s)] r') => Sem r' Int
    getNextIx = do
      idx <- gets @(BuilderState s) (^. stateNextIx)
      modify' @(BuilderState s) (over stateNextIx succ)
      return idx

    addToCurrentBlock :: Sem (Re s r) ()
    addToCurrentBlock = do
      idx <- getNextIx
      whenJust msym $ \(sym :: SymbolType s) -> do
        let psym = symbolParsed sym
        whenJustM (gets @(BuilderState s) (^. stateSymbols . at psym)) (errDuplicateName sym . symbolParsed)
        modify' @(BuilderState s) (set (stateSymbols . at psym) (Just sym))
      let itm =
            NameItem
              { _nameItemDefault = mdef,
                _nameItemSymbol = msym,
                _nameItemImplicit = impl,
                _nameItemIndex = idx,
                _nameItemType = ty
              }
      modify' @(BuilderState s) (over (stateCurrentBlockReverse) (itm :))

    startNewBlock :: Sem (Re s r) ()
    startNewBlock = do
      curBlock <- nonEmpty' . reverse <$> gets @(BuilderState s) (^. stateCurrentBlockReverse)
      mcurImpl <- gets @(BuilderState s) (^. stateCurrentImplicit)
      modify' @(BuilderState s) (set stateCurrentImplicit (Just impl))
      modify' @(BuilderState s) (set stateCurrentBlockReverse mempty)
      modify' @(BuilderState s) (set stateNextIx 0)
      whenJust mcurImpl $ \curImpl ->
        let newBlock = mkNameBlock curBlock curImpl
         in modify' (over stateReverseClosedBlocks (newBlock :))
      addArgument' impl mdef msym ty

endBuild' :: forall s r a. Sem (Re s r) a
endBuild' = get @(BuilderState s) >>= throw

mkRecordNameSignature :: forall s. (SingI s) => RhsRecord s -> RecordNameSignature s
mkRecordNameSignature rhs =
  RecordNameSignature $
    hashMap
      [ ( symbolParsed sym,
          NameItem
            { _nameItemSymbol = Just sym,
              _nameItemIndex,
              _nameItemType = field ^. fieldType,
              _nameItemImplicit = fromIsImplicitField (field ^. fieldIsImplicit),
              _nameItemDefault = Nothing
            }
        )
        | (Indexed _nameItemIndex field) <- indexFrom 0 (toList (rhs ^.. rhsRecordStatements . each . _RecordStatementField)),
          let sym :: SymbolType s = field ^. fieldName
      ]
