module Juvix.Compiler.Core.Data.InfoTable
  ( module Juvix.Compiler.Core.Data.InfoTable,
    module Juvix.Compiler.Concrete.Data.Builtins,
    module Juvix.Compiler.Core.Data.InfoTable.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Compiler.Core.Data.InfoTable.Base
import Juvix.Compiler.Core.Language

type IdentContext = HashMap Symbol Node

type InfoTable = InfoTable' Node

type IdentifierInfo = IdentifierInfo' Node

type InductiveInfo = InductiveInfo' Node

type ConstructorInfo = ConstructorInfo' Node

type AxiomInfo = AxiomInfo' Node

type ParameterInfo = ParameterInfo' Node

type SpecialisationInfo = SpecialisationInfo' Node

nextSymbolId :: InfoTable -> Word
nextSymbolId tab =
  maximum (0 : map (^. symbolId) (HashMap.keys (tab ^. infoIdentifiers)) ++ map (^. symbolId) (HashMap.keys (tab ^. infoInductives)))
    + 1

nextTagId :: InfoTable -> Word
nextTagId tab =
  maximum (0 : mapMaybe getUserTagId (HashMap.keys (tab ^. infoConstructors))) + 1

lookupInductiveInfo' :: InfoTable -> Symbol -> Maybe InductiveInfo
lookupInductiveInfo' tab sym = HashMap.lookup sym (tab ^. infoInductives)

lookupConstructorInfo' :: InfoTable -> Tag -> Maybe ConstructorInfo
lookupConstructorInfo' tab tag = HashMap.lookup tag (tab ^. infoConstructors)

lookupIdentifierInfo' :: InfoTable -> Symbol -> Maybe IdentifierInfo
lookupIdentifierInfo' tab sym = HashMap.lookup sym (tab ^. infoIdentifiers)

lookupIdentifierNode' :: InfoTable -> Symbol -> Maybe Node
lookupIdentifierNode' tab sym = HashMap.lookup sym (tab ^. identContext)

lookupSpecialisationInfo :: InfoTable -> Symbol -> [SpecialisationInfo]
lookupSpecialisationInfo tab sym = fromMaybe [] $ HashMap.lookup sym (tab ^. infoSpecialisations)

lookupInductiveInfo :: InfoTable -> Symbol -> InductiveInfo
lookupInductiveInfo tab sym = fromJust $ lookupInductiveInfo' tab sym

lookupConstructorInfo :: InfoTable -> Tag -> ConstructorInfo
lookupConstructorInfo tab tag = fromMaybe (error ("tag: " <> show tag)) $ lookupConstructorInfo' tab tag

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

-- | Prunes the orphaned entries of identMap, indentContext and
-- infoConstructors, i.e., ones that have no corresponding entries in
-- infoIdentifiers or infoInductives
pruneInfoTable :: InfoTable -> InfoTable
pruneInfoTable tab =
  pruneIdentMap
    $ over
      infoConstructors
      ( HashMap.filter
          ( \ConstructorInfo {..} ->
              HashMap.member _constructorInductive (tab ^. infoInductives)
          )
      )
    $ over
      identContext
      (HashMap.filterWithKey (\s _ -> HashMap.member s (tab ^. infoIdentifiers)))
      tab
  where
    pruneIdentMap :: InfoTable -> InfoTable
    pruneIdentMap tab' =
      over
        identMap
        ( HashMap.filter
            ( \case
                IdentFun s -> HashMap.member s (tab' ^. infoIdentifiers)
                IdentInd s -> HashMap.member s (tab' ^. infoInductives)
                IdentConstr tag -> HashMap.member tag (tab' ^. infoConstructors)
            )
        )
        tab'
