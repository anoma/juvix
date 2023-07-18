module Juvix.Compiler.Concrete.Data.NameSignature.Builder
  ( mkNameSignature,
    HasNameSignature,
    module Juvix.Compiler.Concrete.Data.NameSignature.Base,
    -- to supress unused warning
    getBuilder,
  )
where

import Juvix.Compiler.Concrete.Data.NameSignature.Base
import Juvix.Compiler.Concrete.Data.NameSignature.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Prelude

data NameSignatureBuilder m a where
  AddSymbol :: IsImplicit -> Symbol -> NameSignatureBuilder m ()
  EndBuild :: NameSignatureBuilder m a
  -- | for debugging
  GetBuilder :: NameSignatureBuilder m BuilderState

data BuilderState = BuilderState
  { _stateCurrentImplicit :: Maybe IsImplicit,
    _stateNextIx :: Int,
    -- | maps to itself
    _stateSymbols :: HashMap Symbol Symbol,
    _stateReverseClosedBlocks :: [NameBlock],
    _stateCurrentBlock :: HashMap Symbol (Symbol, Int)
  }

makeLenses ''BuilderState
makeSem ''NameSignatureBuilder

class HasNameSignature d where
  addArgs :: Members '[NameSignatureBuilder] r => d -> Sem r ()

instance HasNameSignature (AxiomDef 'Parsed) where
  addArgs :: Members '[NameSignatureBuilder] r => AxiomDef 'Parsed -> Sem r ()
  addArgs a = addAtoms (a ^. axiomType)

instance HasNameSignature (FunctionDef 'Parsed) where
  addArgs a = do
    mapM_ addSigArg (a ^. signArgs)
    addAtoms (a ^. signRetType)

instance HasNameSignature (InductiveDef 'Parsed, ConstructorDef 'Parsed) where
  addArgs (i, c) = do
    mapM_ addConstructorParams (i ^. inductiveParameters)
    addAtoms (c ^. constructorType)

instance HasNameSignature (InductiveDef 'Parsed) where
  addArgs a = do
    mapM_ addInductiveParams (a ^. inductiveParameters)
    whenJust (a ^. inductiveType) addAtoms

mkNameSignature ::
  (Members '[Error ScoperError] r, HasNameSignature d) =>
  d ->
  Sem r NameSignature
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
      _stateNextIx = 0,
      _stateSymbols = mempty,
      _stateReverseClosedBlocks = [],
      _stateCurrentBlock = mempty
    }

fromBuilderState :: BuilderState -> NameSignature
fromBuilderState b =
  NameSignature
    { _nameSignatureArgs = reverse (addCurrent (b ^. stateReverseClosedBlocks))
    }
  where
    addCurrent :: [NameBlock] -> [NameBlock]
    addCurrent
      | null (b ^. stateCurrentBlock) = id
      | Just i <- b ^. stateCurrentImplicit = (NameBlock (b ^. stateCurrentBlock) i :)
      | otherwise = id

addAtoms :: forall r. Members '[NameSignatureBuilder] r => ExpressionAtoms 'Parsed -> Sem r ()
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
          FunctionParameterName s -> addSymbol _paramImplicit s
          FunctionParameterWildcard {} -> endBuild

addInductiveParams' :: Members '[NameSignatureBuilder] r => IsImplicit -> InductiveParameters 'Parsed -> Sem r ()
addInductiveParams' i a = forM_ (a ^. inductiveParametersNames) (addSymbol i)

addInductiveParams :: Members '[NameSignatureBuilder] r => InductiveParameters 'Parsed -> Sem r ()
addInductiveParams = addInductiveParams' Explicit

addConstructorParams :: Members '[NameSignatureBuilder] r => InductiveParameters 'Parsed -> Sem r ()
addConstructorParams = addInductiveParams' Implicit

addSigArg :: Members '[NameSignatureBuilder] r => SigArg 'Parsed -> Sem r ()
addSigArg a = forM_ (a ^. sigArgNames) (addSymbol (a ^. sigArgImplicit))

type Re r = State BuilderState ': Error BuilderState ': Error NameSignatureError ': r

re ::
  forall r a.
  Sem (NameSignatureBuilder ': r) a ->
  Sem (Re r) a
re = reinterpret3 $ \case
  AddSymbol impl k -> addSymbol' impl k
  EndBuild -> endBuild'
  GetBuilder -> get
{-# INLINE re #-}

addSymbol' :: forall r. IsImplicit -> Symbol -> Sem (Re r) ()
addSymbol' impl sym = do
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
      idx <- (sym,) <$> gets (^. stateNextIx)
      modify' (over stateNextIx succ)
      whenJustM (gets (^. stateSymbols . at sym)) errDuplicateName
      modify' (set (stateSymbols . at sym) (Just sym))
      modify' (set (stateCurrentBlock . at sym) (Just idx))

    startNewBlock :: Sem (Re r) ()
    startNewBlock = do
      curBlock <- gets (^. stateCurrentBlock)
      mcurImpl <- gets (^. stateCurrentImplicit)
      modify' (set stateCurrentImplicit (Just impl))
      modify' (set stateCurrentBlock mempty)
      modify' (set stateNextIx 0)
      whenJust mcurImpl $ \curImpl -> modify' (over stateReverseClosedBlocks (NameBlock curBlock curImpl :))
      addSymbol' impl sym

endBuild' :: Sem (Re r) a
endBuild' = get @BuilderState >>= throw
