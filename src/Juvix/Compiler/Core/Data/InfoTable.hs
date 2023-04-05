module Juvix.Compiler.Core.Data.InfoTable
  ( module Juvix.Compiler.Core.Data.InfoTable,
    module Juvix.Compiler.Concrete.Data.Builtins,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Compiler.Core.Language

type IdentContext = HashMap Symbol Node

data InfoTable = InfoTable
  { _identContext :: IdentContext,
    _identMap :: HashMap Text IdentKind,
    _infoMain :: Maybe Symbol,
    _infoIdentifiers :: HashMap Symbol IdentifierInfo,
    _infoInductives :: HashMap Symbol InductiveInfo,
    _infoConstructors :: HashMap Tag ConstructorInfo,
    _infoAxioms :: HashMap Text AxiomInfo,
    _infoIntToNat :: Maybe Symbol,
    _infoNextSymbol :: Word,
    _infoNextTag :: Word,
    _infoBuiltins :: HashMap BuiltinPrim IdentKind
  }

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _identContext = mempty,
      _identMap = mempty,
      _infoMain = Nothing,
      _infoIdentifiers = mempty,
      _infoInductives = mempty,
      _infoConstructors = mempty,
      _infoAxioms = mempty,
      _infoIntToNat = Nothing,
      _infoNextSymbol = 1,
      _infoNextTag = 0,
      _infoBuiltins = mempty
    }

emptyInfoTable' :: Node -> InfoTable
emptyInfoTable' mainNode =
  emptyInfoTable
    { _identContext = HashMap.singleton 0 mainNode,
      _infoMain = Just 0
    }

data IdentKind
  = IdentFun Symbol
  | IdentInd Symbol
  | IdentConstr Tag

data IdentifierInfo = IdentifierInfo
  { _identifierName :: Text,
    _identifierLocation :: Maybe Location,
    _identifierSymbol :: Symbol,
    _identifierType :: Type,
    -- | The number of lambdas in the identifier body
    _identifierArgsNum :: Int,
    _identifierIsExported :: Bool,
    _identifierBuiltin :: Maybe BuiltinFunction
  }

data InductiveInfo = InductiveInfo
  { _inductiveName :: Text,
    _inductiveLocation :: Maybe Location,
    _inductiveSymbol :: Symbol,
    _inductiveKind :: Type,
    _inductiveConstructors :: [Tag],
    _inductiveParams :: [ParameterInfo],
    _inductivePositive :: Bool,
    _inductiveBuiltin :: Maybe BuiltinType
  }

data ConstructorInfo = ConstructorInfo
  { _constructorName :: Text,
    _constructorLocation :: Maybe Location,
    _constructorTag :: Tag,
    _constructorType :: Type,
    _constructorArgsNum :: Int,
    _constructorInductive :: Symbol,
    _constructorBuiltin :: Maybe BuiltinConstructor
  }

data ParameterInfo = ParameterInfo
  { _paramName :: Text,
    _paramLocation :: Maybe Location,
    _paramKind :: Type,
    _paramIsImplicit :: IsImplicit
  }

data AxiomInfo = AxiomInfo
  { _axiomName :: Text,
    _axiomLocation :: Maybe Location,
    _axiomType :: Type
  }

makeLenses ''InfoTable
makeLenses ''IdentifierInfo
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''ParameterInfo
makeLenses ''AxiomInfo

lookupInductiveInfo' :: InfoTable -> Symbol -> Maybe InductiveInfo
lookupInductiveInfo' tab sym = HashMap.lookup sym (tab ^. infoInductives)

lookupConstructorInfo' :: InfoTable -> Tag -> Maybe ConstructorInfo
lookupConstructorInfo' tab tag = HashMap.lookup tag (tab ^. infoConstructors)

lookupIdentifierInfo' :: InfoTable -> Symbol -> Maybe IdentifierInfo
lookupIdentifierInfo' tab sym = HashMap.lookup sym (tab ^. infoIdentifiers)

lookupIdentifierNode' :: InfoTable -> Symbol -> Maybe Node
lookupIdentifierNode' tab sym = HashMap.lookup sym (tab ^. identContext)

lookupInductiveInfo :: InfoTable -> Symbol -> InductiveInfo
lookupInductiveInfo tab sym = fromJust $ lookupInductiveInfo' tab sym

lookupConstructorInfo :: InfoTable -> Tag -> ConstructorInfo
lookupConstructorInfo tab tag = fromJust $ lookupConstructorInfo' tab tag

lookupIdentifierInfo :: InfoTable -> Symbol -> IdentifierInfo
lookupIdentifierInfo tab sym = fromJust $ lookupIdentifierInfo' tab sym

lookupIdentifierNode :: InfoTable -> Symbol -> Node
lookupIdentifierNode tab sym = fromJust $ lookupIdentifierNode' tab sym

lookupBuiltinInductive :: InfoTable -> BuiltinInductive -> Maybe InductiveInfo
lookupBuiltinInductive tab b = (HashMap.!) (tab ^. infoInductives) . indSym <$> idenKind
  where
    idenKind :: Maybe IdentKind
    idenKind = HashMap.lookup (BuiltinsInductive b) (tab ^. infoBuiltins)

    indSym :: IdentKind -> Symbol
    indSym = \case
      IdentInd s -> s
      _ -> error "core infotable: expected inductive identifier"

lookupBuiltinConstructor :: InfoTable -> BuiltinConstructor -> Maybe ConstructorInfo
lookupBuiltinConstructor tab b = (HashMap.!) (tab ^. infoConstructors) . ctorTag <$> idenKind
  where
    idenKind :: Maybe IdentKind
    idenKind = HashMap.lookup (BuiltinsConstructor b) (tab ^. infoBuiltins)

    ctorTag :: IdentKind -> Tag
    ctorTag = \case
      IdentConstr t -> t
      _ -> error "core infotable: expected constructor identifier"

lookupBuiltinFunction :: InfoTable -> BuiltinFunction -> Maybe IdentifierInfo
lookupBuiltinFunction tab b = (HashMap.!) (tab ^. infoIdentifiers) . funSym <$> idenKind
  where
    idenKind :: Maybe IdentKind
    idenKind = HashMap.lookup (BuiltinsFunction b) (tab ^. infoBuiltins)

    funSym :: IdentKind -> Symbol
    funSym = \case
      IdentFun s -> s
      _ -> error "core infotable: expected function identifier"

identName :: InfoTable -> Symbol -> Text
identName tab sym = lookupIdentifierInfo tab sym ^. identifierName

typeName :: InfoTable -> Symbol -> Text
typeName tab sym = lookupInductiveInfo tab sym ^. inductiveName

identNames :: InfoTable -> HashSet Text
identNames tab =
  HashSet.fromList $
    map (^. identifierName) (HashMap.elems (tab ^. infoIdentifiers))
      ++ map (^. constructorName) (HashMap.elems (tab ^. infoConstructors))
      ++ map (^. inductiveName) (HashMap.elems (tab ^. infoInductives))

freshIdentName :: InfoTable -> Text -> Text
freshIdentName tab = freshName (identNames tab)

filterByFile :: Path Abs File -> InfoTable -> InfoTable
filterByFile f t =
  t
    { _infoIdentifiers = HashMap.filter (^. identifierLocation . to matchesLocation) (t ^. infoIdentifiers),
      _infoAxioms = HashMap.filter (^. axiomLocation . to matchesLocation) (t ^. infoAxioms),
      _infoConstructors = HashMap.filter (^. constructorLocation . to matchesLocation) (t ^. infoConstructors),
      _infoInductives = HashMap.filter (^. inductiveLocation . to matchesLocation) (t ^. infoInductives)
    }
  where
    matchesLocation :: Maybe Location -> Bool
    matchesLocation l = l ^? _Just . intervalFile == Just f

-- | Prunes the orphaned entries of identMap and indentContext, i.e., ones that
-- have no corresponding entries in infoIdentifiers or infoInductives
pruneInfoTable :: InfoTable -> InfoTable
pruneInfoTable tab =
  over
    identMap
    ( HashMap.filter
        ( \case
            IdentFun s -> HashMap.member s (tab ^. infoIdentifiers)
            IdentInd s -> HashMap.member s (tab ^. infoInductives)
            IdentConstr tag -> HashMap.member tag (tab ^. infoConstructors)
        )
    )
    $ over
      identContext
      (HashMap.filterWithKey (\s _ -> HashMap.member s (tab ^. infoIdentifiers)))
      tab
