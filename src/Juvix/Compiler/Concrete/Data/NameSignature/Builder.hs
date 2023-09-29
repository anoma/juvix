module Juvix.Compiler.Concrete.Data.NameSignature.Builder
  ( mkNameSignature,
    mkRecordNameSignature,
    HasNameSignature,
    -- to supress unused warning
    getBuilder,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.NameSignature.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Prelude

data NameSignatureBuilder m a where
  AddSymbol :: IsImplicit -> Maybe (ArgDefault 'Parsed) -> Symbol -> NameSignatureBuilder m ()
  EndBuild :: NameSignatureBuilder m a
  -- | for debugging
  GetBuilder :: NameSignatureBuilder m BuilderState

data BuilderState = BuilderState
  { _stateCurrentImplicit :: Maybe IsImplicit,
    _stateCurrentDefault :: Maybe (ArgDefault 'Parsed),
    _stateNextIx :: Int,
    -- | maps to itself
    _stateSymbols :: HashMap Symbol Symbol,
    _stateReverseClosedBlocks :: [NameBlock 'Parsed],
    _stateCurrentBlock :: HashMap Symbol NameItem
  }

makeLenses ''BuilderState
makeSem ''NameSignatureBuilder

class HasNameSignature d where
  addArgs :: (Members '[NameSignatureBuilder] r) => d -> Sem r ()

instance HasNameSignature (AxiomDef 'Parsed) where
  addArgs :: (Members '[NameSignatureBuilder] r) => AxiomDef 'Parsed -> Sem r ()
  addArgs a = addAtoms (a ^. axiomType)

instance HasNameSignature (FunctionDef 'Parsed) where
  addArgs a = do
    mapM_ addSigArg (a ^. signArgs)
    whenJust (a ^. signRetType) addAtoms

instance HasNameSignature (InductiveDef 'Parsed, ConstructorDef 'Parsed) where
  addArgs ::
    forall r.
    (Members '[NameSignatureBuilder] r) =>
    (InductiveDef 'Parsed, ConstructorDef 'Parsed) ->
    Sem r ()
  addArgs (i, c) = do
    mapM_ addConstructorParams (i ^. inductiveParameters)
    addRhs (c ^. constructorRhs)
    where
      addRecord :: RhsRecord 'Parsed -> Sem r ()
      addRecord RhsRecord {..} = mapM_ addField _rhsRecordFields
        where
          addField :: RecordField 'Parsed -> Sem r ()
          addField RecordField {..} = addSymbol Explicit Nothing _fieldName
      addRhs :: ConstructorRhs 'Parsed -> Sem r ()
      addRhs = \case
        ConstructorRhsGadt g -> addAtoms (g ^. rhsGadtType)
        ConstructorRhsRecord g -> addRecord g
        ConstructorRhsAdt {} -> return ()

instance HasNameSignature (InductiveDef 'Parsed) where
  addArgs a = do
    mapM_ addInductiveParams (a ^. inductiveParameters)
    whenJust (a ^. inductiveType) addAtoms

mkNameSignature ::
  (Members '[Error ScoperError] r, HasNameSignature d) =>
  d ->
  Sem r (NameSignature 'Parsed)
mkNameSignature d = do
  fmap (fromBuilderState . fromLeft impossible)
    . mapError ErrNameSignature
    . runError @BuilderState
    . evalState iniBuilderState
    . re
    $ do
      addArgs d
      endBuild

iniBuilderState :: BuilderState
iniBuilderState =
  BuilderState
    { _stateCurrentImplicit = Nothing,
      _stateCurrentDefault = Nothing,
      _stateNextIx = 0,
      _stateSymbols = mempty,
      _stateReverseClosedBlocks = [],
      _stateCurrentBlock = mempty
    }

fromBuilderState :: BuilderState -> NameSignature 'Parsed
fromBuilderState b =
  NameSignature
    { _nameSignatureArgs = reverse (addCurrent (b ^. stateReverseClosedBlocks))
    }
  where
    addCurrent :: [NameBlock 'Parsed] -> [NameBlock 'Parsed]
    addCurrent
      | null (b ^. stateCurrentBlock) = id
      | Just i <- b ^. stateCurrentImplicit =
          (NameBlock (b ^. stateCurrentBlock) i (b ^? stateCurrentDefault . _Just . argDefaultValue) :)
      | otherwise = id

addAtoms :: forall r. (Members '[NameSignatureBuilder] r) => ExpressionAtoms 'Parsed -> Sem r ()
addAtoms atoms = addAtom . (^. expressionAtoms . _head1) $ atoms
  where
    addAtom :: ExpressionAtom 'Parsed -> Sem r ()
    addAtom = \case
      AtomFunction f -> do
        addParameters (f ^. funParameters)
        addAtoms (f ^. funReturn)
      _ -> endBuild

    addParameters :: FunctionParameters 'Parsed -> Sem r ()
    addParameters FunctionParameters {..} = forM_ _paramNames addParameter
      where
        addParameter :: FunctionParameter 'Parsed -> Sem r ()
        addParameter = \case
          -- TODO add default values to Pi types?
          FunctionParameterName s -> addSymbol _paramImplicit Nothing s
          FunctionParameterWildcard {} -> endBuild

addInductiveParams' :: (Members '[NameSignatureBuilder] r) => IsImplicit -> InductiveParameters 'Parsed -> Sem r ()
addInductiveParams' i a = forM_ (a ^. inductiveParametersNames) (addSymbol i Nothing)

addInductiveParams :: (Members '[NameSignatureBuilder] r) => InductiveParameters 'Parsed -> Sem r ()
addInductiveParams = addInductiveParams' Explicit

addConstructorParams :: (Members '[NameSignatureBuilder] r) => InductiveParameters 'Parsed -> Sem r ()
addConstructorParams = addInductiveParams' Implicit

addSigArg :: (Members '[NameSignatureBuilder] r) => SigArg 'Parsed -> Sem r ()
addSigArg a = forM_ (a ^. sigArgNames) $ \case
  ArgumentSymbol s -> addSymbol (a ^. sigArgImplicit) (a ^. sigArgDefault) s
  ArgumentWildcard {} -> return ()

type Re r = State BuilderState ': Error BuilderState ': Error NameSignatureError ': r

re ::
  forall r a.
  Sem (NameSignatureBuilder ': r) a ->
  Sem (Re r) a
re = reinterpret3 $ \case
  AddSymbol impl mdef k -> addSymbol' impl mdef k
  EndBuild -> endBuild'
  GetBuilder -> get
{-# INLINE re #-}

addSymbol' :: forall r. IsImplicit -> Maybe (ArgDefault 'Parsed) -> Symbol -> Sem (Re r) ()
addSymbol' impl mdef sym = do
  curImpl <- gets (^. stateCurrentImplicit)
  if
      | Just impl == curImpl -> addToCurrentBlock
      | otherwise -> startNewBlock
  where
    errDuplicateName :: Symbol -> Sem (Re r) ()
    errDuplicateName _dupNameFirst =
      throw $
        ErrDuplicateName
          DuplicateName
            { _dupNameSecond = sym,
              ..
            }

    addToCurrentBlock :: Sem (Re r) ()
    addToCurrentBlock = do
      idx <- gets (^. stateNextIx)
      let itm = NameItem sym idx
      modify' (over stateNextIx succ)
      whenJustM (gets (^. stateSymbols . at sym)) errDuplicateName
      modify' (set (stateSymbols . at sym) (Just sym))
      modify' (set (stateCurrentBlock . at sym) (Just itm))

    startNewBlock :: Sem (Re r) ()
    startNewBlock = do
      curBlock <- gets (^. stateCurrentBlock)
      mcurImpl <- gets (^. stateCurrentImplicit)
      mcurDef <- fmap (^. argDefaultValue) <$> gets (^. stateCurrentDefault)
      modify' (set stateCurrentImplicit (Just impl))
      modify' (set stateCurrentDefault mdef)
      modify' (set stateCurrentBlock mempty)
      modify' (set stateNextIx 0)
      whenJust mcurImpl $ \curImpl ->
        modify' (over stateReverseClosedBlocks (NameBlock curBlock curImpl mcurDef :))
      addSymbol' impl mdef sym

endBuild' :: Sem (Re r) a
endBuild' = get @BuilderState >>= throw

mkRecordNameSignature :: RhsRecord 'Parsed -> RecordNameSignature
mkRecordNameSignature rhs =
  RecordNameSignature
    ( HashMap.fromList
        [ (s, NameItem s idx) | (Indexed idx field) <- indexFrom 0 (toList (rhs ^. rhsRecordFields)), let s = field ^. fieldName
        ]
    )
