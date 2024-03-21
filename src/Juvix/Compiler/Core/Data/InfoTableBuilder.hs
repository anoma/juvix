module Juvix.Compiler.Core.Data.InfoTableBuilder
  ( module Juvix.Compiler.Core.Data.InfoTable,
    module Juvix.Compiler.Core.Data.InfoTableBuilder,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.Module
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language

data InfoTableBuilder :: Effect where
  FreshSymbol :: InfoTableBuilder m Symbol
  FreshTag :: InfoTableBuilder m Tag
  RegisterIdent :: Text -> IdentifierInfo -> InfoTableBuilder m ()
  RegisterConstructor :: Text -> ConstructorInfo -> InfoTableBuilder m ()
  RegisterInductive :: Text -> InductiveInfo -> InfoTableBuilder m ()
  RegisterSpecialisation :: Symbol -> SpecialisationInfo -> InfoTableBuilder m ()
  RegisterIdentNode :: Symbol -> Node -> InfoTableBuilder m ()
  RegisterMain :: Symbol -> InfoTableBuilder m ()
  RegisterLiteralIntToNat :: Symbol -> InfoTableBuilder m ()
  RegisterLiteralIntToInt :: Symbol -> InfoTableBuilder m ()
  RemoveSymbol :: Symbol -> InfoTableBuilder m ()
  OverIdentArgs :: Symbol -> ([Binder] -> [Binder]) -> InfoTableBuilder m ()
  GetIdent :: Text -> InfoTableBuilder m (Maybe IdentKind)
  GetModule :: InfoTableBuilder m Module
  SetModule :: Module -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

getConstructorInfo :: (Member InfoTableBuilder r) => Tag -> Sem r ConstructorInfo
getConstructorInfo tag = flip lookupConstructorInfo tag <$> getModule

getInductiveInfo :: (Member InfoTableBuilder r) => Symbol -> Sem r InductiveInfo
getInductiveInfo sym = flip lookupInductiveInfo sym <$> getModule

getBuiltinInductiveInfo :: (Member InfoTableBuilder r) => BuiltinInductive -> Sem r InductiveInfo
getBuiltinInductiveInfo b = do
  tab <- getModule
  return $ fromJust (lookupBuiltinInductive tab b)

getIdentifierInfo :: (Member InfoTableBuilder r) => Symbol -> Sem r IdentifierInfo
getIdentifierInfo sym = flip lookupIdentifierInfo sym <$> getModule

getBoolSymbol :: (Member InfoTableBuilder r) => Sem r Symbol
getBoolSymbol = do
  ci <- getConstructorInfo (BuiltinTag TagTrue)
  return $ ci ^. constructorInductive

getIOSymbol :: (Member InfoTableBuilder r) => Sem r Symbol
getIOSymbol = do
  ci <- getConstructorInfo (BuiltinTag TagWrite)
  return $ ci ^. constructorInductive

getNatSymbol :: (Member InfoTableBuilder r) => Sem r Symbol
getNatSymbol = (^. inductiveSymbol) <$> getBuiltinInductiveInfo BuiltinNat

getIntSymbol :: (Member InfoTableBuilder r) => Sem r Symbol
getIntSymbol = (^. inductiveSymbol) <$> getBuiltinInductiveInfo BuiltinInt

checkSymbolDefined :: (Member InfoTableBuilder r) => Symbol -> Sem r Bool
checkSymbolDefined sym = do
  m <- getModule
  return $
    HashMap.member sym (m ^. moduleInfoTable . identContext)
      || HashMap.member sym (m ^. moduleImportsTable . identContext)

setIdentArgs :: (Member InfoTableBuilder r) => Symbol -> [Binder] -> Sem r ()
setIdentArgs sym = overIdentArgs sym . const

data BuilderState = BuilderState
  { _builderStateModule :: Module,
    _builderStateNextSymbolId :: Word,
    _builderStateNextTagId :: Word
  }

makeLenses ''BuilderState

mkBuilderState :: Module -> BuilderState
mkBuilderState m =
  BuilderState
    { _builderStateModule = m,
      _builderStateNextSymbolId = nextSymbolId tab,
      _builderStateNextTagId = nextTagId tab
    }
  where
    tab = computeCombinedInfoTable m

runInfoTableBuilder' :: BuilderState -> forall r a. Sem (InfoTableBuilder ': r) a -> Sem r (BuilderState, a)
runInfoTableBuilder' st = reinterpret (runState st) interp
  where
    interp :: InfoTableBuilder m b -> Sem (State BuilderState ': r) b
    interp = \case
      FreshSymbol -> do
        s <- get
        modify' (over builderStateNextSymbolId (+ 1))
        return (Symbol (s ^. builderStateModule . moduleId) (s ^. builderStateNextSymbolId))
      FreshTag -> do
        s <- get
        modify' (over builderStateNextTagId (+ 1))
        return (UserTag (TagUser (s ^. builderStateModule . moduleId) (s ^. builderStateNextTagId)))
      RegisterIdent idt ii -> do
        let sym = ii ^. identifierSymbol
            identKind = IdentFun (ii ^. identifierSymbol)
        whenJust
          (ii ^. identifierBuiltin)
          (\b -> modify' (over (builderStateModule . moduleInfoTable . infoBuiltins) (HashMap.insert (BuiltinsFunction b) identKind)))
        modify' (over (builderStateModule . moduleInfoTable . infoIdentifiers) (HashMap.insert sym ii))
        modify' (over (builderStateModule . moduleInfoTable . identMap) (HashMap.insert idt identKind))
      RegisterConstructor idt ci -> do
        let tag = ci ^. constructorTag
            identKind = IdentConstr tag
        whenJust
          (ci ^. constructorBuiltin)
          (\b -> modify' (over (builderStateModule . moduleInfoTable . infoBuiltins) (HashMap.insert (BuiltinsConstructor b) identKind)))
        modify' (over (builderStateModule . moduleInfoTable . infoConstructors) (HashMap.insert tag ci))
        modify' (over (builderStateModule . moduleInfoTable . identMap) (HashMap.insert idt identKind))
      RegisterInductive idt ii -> do
        let sym = ii ^. inductiveSymbol
            identKind = IdentInd sym
        whenJust
          (ii ^. inductiveBuiltin)
          (\b -> modify' (over (builderStateModule . moduleInfoTable . infoBuiltins) (HashMap.insert (builtinTypeToPrim b) identKind)))
        modify' (over (builderStateModule . moduleInfoTable . infoInductives) (HashMap.insert sym ii))
        modify' (over (builderStateModule . moduleInfoTable . identMap) (HashMap.insert idt identKind))
      RegisterSpecialisation sym spec -> do
        modify'
          ( over
              (builderStateModule . moduleInfoTable . infoSpecialisations)
              (HashMap.alter (Just . maybe [spec] (spec :)) sym)
          )
      RegisterIdentNode sym node ->
        modify' (over (builderStateModule . moduleInfoTable . identContext) (HashMap.insert sym node))
      RegisterMain sym -> do
        modify' (set (builderStateModule . moduleInfoTable . infoMain) (Just sym))
      RegisterLiteralIntToInt sym -> do
        modify' (set (builderStateModule . moduleInfoTable . infoLiteralIntToInt) (Just sym))
      RegisterLiteralIntToNat sym -> do
        modify' (set (builderStateModule . moduleInfoTable . infoLiteralIntToNat) (Just sym))
      RemoveSymbol sym -> do
        modify' (over (builderStateModule . moduleInfoTable . infoMain) (maybe Nothing (\sym' -> if sym' == sym then Nothing else Just sym')))
        modify' (over (builderStateModule . moduleInfoTable . infoIdentifiers) (HashMap.delete sym))
        modify' (over (builderStateModule . moduleInfoTable . identContext) (HashMap.delete sym))
        modify' (over (builderStateModule . moduleInfoTable . infoInductives) (HashMap.delete sym))
      OverIdentArgs sym f -> do
        args <- f <$> gets (^. builderStateModule . moduleInfoTable . identContext . at sym . _Just . to (map (^. lambdaLhsBinder) . fst . unfoldLambdas))
        modify' (set (builderStateModule . moduleInfoTable . infoIdentifiers . at sym . _Just . identifierArgsNum) (length args))
        modify' (over (builderStateModule . moduleInfoTable . infoIdentifiers) (HashMap.adjust (over identifierType (expandType args)) sym))
      GetIdent txt -> do
        s <- get
        let r1 = HashMap.lookup txt (s ^. builderStateModule . moduleInfoTable . identMap)
            r2 = HashMap.lookup txt (s ^. builderStateModule . moduleImportsTable . identMap)
        return (r1 <|> r2)
      GetModule ->
        (^. builderStateModule) <$> get
      SetModule md ->
        modify' (set builderStateModule md)

execInfoTableBuilder' :: BuilderState -> Sem (InfoTableBuilder ': r) a -> Sem r BuilderState
execInfoTableBuilder' st = fmap fst . runInfoTableBuilder' st

evalInfoTableBuilder' :: BuilderState -> Sem (InfoTableBuilder ': r) a -> Sem r a
evalInfoTableBuilder' st = fmap snd . runInfoTableBuilder' st

runInfoTableBuilder :: Module -> Sem (InfoTableBuilder ': r) a -> Sem r (Module, a)
runInfoTableBuilder m ma = do
  (st, a) <- runInfoTableBuilder' (mkBuilderState m) ma
  return (st ^. builderStateModule, a)

execInfoTableBuilder :: Module -> Sem (InfoTableBuilder ': r) a -> Sem r Module
execInfoTableBuilder m = fmap fst . runInfoTableBuilder m

evalInfoTableBuilder :: Module -> Sem (InfoTableBuilder ': r) a -> Sem r a
evalInfoTableBuilder m = fmap snd . runInfoTableBuilder m

--------------------------------------------
-- Builtin declarations
--------------------------------------------

createBuiltinConstr ::
  Symbol ->
  Tag ->
  Text ->
  Type ->
  Maybe BuiltinConstructor ->
  ConstructorInfo
createBuiltinConstr sym tag nameTxt ty cblt =
  ConstructorInfo
    { _constructorName = nameTxt,
      _constructorLocation = Nothing,
      _constructorTag = tag,
      _constructorType = ty,
      _constructorArgsNum = argsNum,
      _constructorArgNames = replicate argsNum Nothing,
      _constructorInductive = sym,
      _constructorFixity = Nothing,
      _constructorBuiltin = cblt,
      _constructorPragmas = mempty
    }
  where
    argsNum = length (typeArgs ty)

builtinConstrs ::
  Symbol ->
  Type ->
  [(Tag, Text, Type -> Type, Maybe BuiltinConstructor)] ->
  [ConstructorInfo]
builtinConstrs sym ty ctrs =
  map (\(tag, name, fty, cblt) -> createBuiltinConstr sym tag name (fty ty) cblt) ctrs

declareInductiveBuiltins ::
  (Member InfoTableBuilder r) =>
  Text ->
  Maybe BuiltinType ->
  [(Tag, Text, Type -> Type, Maybe BuiltinConstructor)] ->
  Sem r ()
declareInductiveBuiltins indName blt ctrs = do
  sym <- freshSymbol
  let ty = mkTypeConstr (setInfoName indName mempty) sym []
      constrs = builtinConstrs sym ty ctrs
  registerInductive
    indName
    ( InductiveInfo
        { _inductiveName = indName,
          _inductiveLocation = Nothing,
          _inductiveSymbol = sym,
          _inductiveKind = mkDynamic',
          _inductiveConstructors = map (^. constructorTag) constrs,
          _inductivePositive = True,
          _inductiveParams = [],
          _inductiveBuiltin = blt,
          _inductivePragmas = mempty
        }
    )
  mapM_ (\ci -> registerConstructor (ci ^. constructorName) ci) constrs

builtinIOConstrs :: [(Tag, Text, Type -> Type, Maybe BuiltinConstructor)]
builtinIOConstrs =
  [ (BuiltinTag TagReturn, "return", mkPi' mkDynamic', Nothing),
    (BuiltinTag TagBind, "bind", \ty -> mkPi' ty (mkPi' (mkPi' mkDynamic' ty) ty), Nothing),
    (BuiltinTag TagWrite, "write", mkPi' mkDynamic', Nothing),
    (BuiltinTag TagReadLn, "readLn", id, Nothing)
  ]

declareIOBuiltins :: (Member InfoTableBuilder r) => Sem r ()
declareIOBuiltins =
  declareInductiveBuiltins
    "IO"
    (Just (BuiltinTypeAxiom BuiltinIO))
    builtinIOConstrs

declareBoolBuiltins :: (Member InfoTableBuilder r) => Sem r ()
declareBoolBuiltins =
  declareInductiveBuiltins
    "Bool"
    (Just (BuiltinTypeInductive BuiltinBool))
    [ (BuiltinTag TagTrue, "true", const mkTypeBool', Just BuiltinBoolTrue),
      (BuiltinTag TagFalse, "false", const mkTypeBool', Just BuiltinBoolFalse)
    ]

declareNatBuiltins :: (Member InfoTableBuilder r) => Sem r ()
declareNatBuiltins = do
  tagZero <- freshTag
  tagSuc <- freshTag
  declareInductiveBuiltins
    "Nat"
    (Just (BuiltinTypeInductive BuiltinNat))
    [ (tagZero, "zero", id, Just BuiltinNatZero),
      (tagSuc, "suc", \x -> mkPi' x x, Just BuiltinNatSuc)
    ]

reserveLiteralIntToNatSymbol :: (Member InfoTableBuilder r) => Sem r ()
reserveLiteralIntToNatSymbol = do
  sym <- freshSymbol
  registerLiteralIntToNat sym

reserveLiteralIntToIntSymbol :: (Member InfoTableBuilder r) => Sem r ()
reserveLiteralIntToIntSymbol = do
  sym <- freshSymbol
  registerLiteralIntToInt sym

-- | Register a function Int -> Nat used to transform literal integers to builtin Nat
setupLiteralIntToNat :: forall r. (Member InfoTableBuilder r) => (Symbol -> Sem r Node) -> Sem r ()
setupLiteralIntToNat mkNode = do
  m <- getModule
  whenJust (getInfoLiteralIntToNat m) go
  where
    go :: Symbol -> Sem r ()
    go sym = do
      ii <- info sym
      registerIdent (ii ^. identifierName) ii
      n <- mkNode sym
      registerIdentNode sym n
      where
        info :: Symbol -> Sem r IdentifierInfo
        info s = do
          m <- getModule
          ty <- targetType
          return $
            IdentifierInfo
              { _identifierSymbol = s,
                _identifierName = freshIdentName m "intToNat",
                _identifierLocation = Nothing,
                _identifierArgsNum = 1,
                _identifierType = mkPi mempty (Binder "x" Nothing mkTypeInteger') ty,
                _identifierIsExported = False,
                _identifierBuiltin = Nothing,
                _identifierPragmas = mempty,
                _identifierArgNames = [Just "x"]
              }

        targetType :: Sem r Node
        targetType = do
          m <- getModule
          let natSymM = (^. inductiveSymbol) <$> lookupBuiltinInductive m BuiltinNat
          return (maybe mkTypeInteger' (\s -> mkTypeConstr (setInfoName "Nat" mempty) s []) natSymM)

-- | Register a function Int -> Int used to transform literal integers to builtin Int
setupLiteralIntToInt :: forall r. (Member InfoTableBuilder r) => Sem r Node -> Sem r ()
setupLiteralIntToInt node = do
  m <- getModule
  whenJust (getInfoLiteralIntToInt m) go
  where
    go :: Symbol -> Sem r ()
    go sym = do
      ii <- info sym
      registerIdent (ii ^. identifierName) ii
      n <- node
      registerIdentNode sym n
      where
        info :: Symbol -> Sem r IdentifierInfo
        info s = do
          m <- getModule
          ty <- targetType
          return $
            IdentifierInfo
              { _identifierSymbol = s,
                _identifierName = freshIdentName m "literalIntToInt",
                _identifierLocation = Nothing,
                _identifierArgsNum = 1,
                _identifierType = mkPi mempty (Binder "x" Nothing mkTypeInteger') ty,
                _identifierIsExported = False,
                _identifierBuiltin = Nothing,
                _identifierPragmas = mempty,
                _identifierArgNames = [Just "x"]
              }

        targetType :: Sem r Node
        targetType = do
          m <- getModule
          let intSymM = (^. inductiveSymbol) <$> lookupBuiltinInductive m BuiltinInt
          return (maybe mkTypeInteger' (\s -> mkTypeConstr (setInfoName "Int" mempty) s []) intSymM)
