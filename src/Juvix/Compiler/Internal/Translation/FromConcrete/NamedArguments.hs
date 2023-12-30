module Juvix.Compiler.Internal.Translation.FromConcrete.NamedArguments
  ( runNamedArguments,
    DesugaredNamedApplication,
    dnamedAppIdentifier,
    dnamedAppArgs,
    dnamedExtraArgs,
    Arg,
    argName,
    argImplicit,
    argAutoInserted,
    argValue,
    argType,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra (symbolParsed)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Internal.Extra.Base qualified as Internal
import Juvix.Prelude

type NameSignatures = HashMap S.NameId (NameSignature 'Scoped)

data BuilderState = BuilderState
  { _stateRemainingArgs :: [ArgumentBlock 'Scoped],
    _stateRemainingNames :: [NameBlock 'Scoped]
  }

data Arg = Arg
  { _argName :: S.Symbol,
    _argImplicit :: IsImplicit,
    _argType :: Expression,
    _argAutoInserted :: Bool,
    _argValue :: Expression
  }

-- | The result of desugaring a named application
data DesugaredNamedApplication = DesugaredNamedApplication
  { _dnamedAppIdentifier :: ScopedIden,
    _dnamedAppArgs :: NonEmpty Arg,
    _dnamedExtraArgs :: [Internal.ApplicationArg]
  }

makeLenses ''BuilderState
makeLenses ''Arg
makeLenses ''DesugaredNamedApplication

runNamedArguments ::
  forall r.
  (Members '[NameIdGen, Error ScoperError, Reader NameSignatures] r) =>
  NamedApplication 'Scoped ->
  [Internal.ApplicationArg] ->
  Sem r DesugaredNamedApplication
runNamedArguments napp extraArgs = do
  iniSt <- mkIniBuilderState
  namedArgs <-
    fmap nonEmpty'
      . execOutputList
      . mapError ErrNamedArgumentsError
      . execState iniSt
      $ helper (getLoc napp)
  return
    DesugaredNamedApplication
      { _dnamedAppIdentifier = napp ^. namedAppName,
        _dnamedAppArgs = namedArgs,
        _dnamedExtraArgs = extraArgs
      }
  where
    mkIniBuilderState :: Sem r BuilderState
    mkIniBuilderState = do
      let name = napp ^. namedAppName . scopedIdenName
      msig <- asks @NameSignatures (^. at (name ^. S.nameId))
      let sig = fromMaybe err msig
            where
              err = error ("impossible: could not find name signature for " <> prettyText name)
      return
        BuilderState
          { _stateRemainingArgs = toList (napp ^. namedAppArgs),
            _stateRemainingNames = sig ^. nameSignatureArgs
          }

type NamesByIndex = IntMap (NameItem 'Scoped)

mkNamesIndex :: [NameItem 'Scoped] -> NamesByIndex
mkNamesIndex = indexedByInt (^. nameItemIndex)

helper ::
  forall r.
  (Members '[State BuilderState, Output Arg, NameIdGen, Error NamedArgumentsError] r) =>
  Interval ->
  Sem r ()
helper loc = do
  whenJustM nextArgumentGroup $ \(impl, args, isLastBlock) -> do
    checkRepeated args
    names :: [NameItem 'Scoped] <- nextNameGroup impl

    (pendingArgs, (omittedNames, argmap)) <- scanGroup impl names args
    emitArgs impl isLastBlock (mkNamesIndex names) omittedNames argmap
    whenJust (nonEmpty pendingArgs) $ \pendingArgs' -> do
      sig <- nextNameGroup Implicit
      emitImplicit False (mkNamesIndex sig) sig mempty
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
              namesByIx = mkNamesIndex (toList (b ^. nameBlock))
          modify' (set stateRemainingNames bs)
          let r = toList (b ^. nameBlock)
              matches = return r
          case (implArgs, implSig) of
            (Explicit, Explicit) -> matches
            (Implicit, Implicit) -> matches
            (ImplicitInstance, ImplicitInstance) -> matches
            (Explicit, Implicit) -> do
              emitImplicit False namesByIx r mempty
              nextNameGroup implArgs
            (Explicit, ImplicitInstance) -> do
              emitImplicitInstance False namesByIx r mempty
              nextNameGroup implArgs
            (Implicit, ImplicitInstance) -> do
              emitImplicitInstance False namesByIx r mempty
              nextNameGroup implArgs
            (ImplicitInstance, Implicit) -> do
              emitImplicit False namesByIx r mempty
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

    emitArgs :: IsImplicit -> Bool -> NamesByIndex -> [NameItem 'Scoped] -> IntMap Arg -> Sem r ()
    emitArgs = \case
      Implicit -> emitImplicit
      Explicit -> emitExplicit
      ImplicitInstance -> emitImplicitInstance
      where
        -- omitting arguments is only allowed at the end
        emitExplicit :: Bool -> NamesByIndex -> [NameItem 'Scoped] -> IntMap Arg -> Sem r ()
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

            missingErr :: NonEmpty (SymbolType 'Scoped) -> Sem r ()
            missingErr = throw . ErrMissingArguments . MissingArguments loc

    emitImplicitHelper ::
      IsImplicit ->
      (HoleType 'Scoped -> Expression) ->
      Bool ->
      NamesByIndex ->
      [NameItem 'Scoped] ->
      IntMap Arg ->
      Sem r ()
    emitImplicitHelper impl exprHole lastBlock namesByIx omittedArgs args = go 0 (IntMap.toAscList args)
      where
        go :: Int -> [(Int, Arg)] -> Sem r ()
        go n = \case
          []
            | lastBlock -> return ()
            | otherwise -> whenJust maxIx (fillUntil . succ)
          (n', a) : rest -> do
            fillUntil n'
            output a
            go (n' + 1) rest
          where
            fillUntil n' = forM_ [n .. n' - 1] (fillPosition >=> output)

            fillPosition :: (Members '[NameIdGen] r') => Int -> Sem r' Arg
            fillPosition idx = do
              let nm :: NameItem 'Scoped = namesByIx ^?! at idx . _Just
              _argValue <- case nm ^. nameItemDefault of
                Nothing -> exprHole . mkHole loc <$> freshNameId
                -- TODO update location
                Just d -> return (d ^. argDefaultValue)

              return
                Arg
                  { _argName = nm ^. nameItemSymbol,
                    _argImplicit = impl,
                    _argType = nm ^. nameItemType,
                    _argAutoInserted = True,
                    _argValue
                  }
        maxIx :: Maybe Int
        maxIx = fmap maximum1 . nonEmpty . map (^. nameItemIndex) $ omittedArgs

    emitImplicit :: Bool -> NamesByIndex -> [NameItem 'Scoped] -> IntMap Arg -> Sem r ()
    emitImplicit = emitImplicitHelper Implicit ExpressionHole

    emitImplicitInstance :: Bool -> NamesByIndex -> [NameItem 'Scoped] -> IntMap Arg -> Sem r ()
    emitImplicitInstance = emitImplicitHelper ImplicitInstance ExpressionInstanceHole

    scanGroup ::
      IsImplicit ->
      [NameItem 'Scoped] ->
      [NamedArgument 'Scoped] ->
      Sem r ([NamedArgument 'Scoped], ([NameItem 'Scoped], IntMap Arg))
    scanGroup impl names =
      fmap (second (first toList))
        . runOutputList
        . runState namesBySymbol
        . execState mempty
        . mapM_ go
      where
        namesBySymbol :: HashMap Symbol (NameItem 'Scoped)
        namesBySymbol = HashMap.fromList [(symbolParsed (i ^. nameItemSymbol), i) | i <- names]
        go ::
          (Members '[State (IntMap Arg), State (HashMap Symbol (NameItem 'Scoped)), State BuilderState, Output (NamedArgument 'Scoped), Error NamedArgumentsError] r') =>
          NamedArgument 'Scoped ->
          Sem r' ()
        go arg = do
          let sym = arg ^. namedArgName
          midx :: Maybe (NameItem 'Scoped) <- gets @(HashMap Symbol (NameItem 'Scoped)) (^. at sym)
          case midx of
            Just idx -> do
              let newArg =
                    Arg
                      { _argName = idx ^. nameItemSymbol,
                        _argValue = arg ^. namedArgValue,
                        _argAutoInserted = False,
                        _argType = idx ^. nameItemType,
                        _argImplicit = impl
                      }
              modify' (IntMap.insert (idx ^. nameItemIndex) newArg)
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
