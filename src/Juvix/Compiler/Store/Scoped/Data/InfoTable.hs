module Juvix.Compiler.Store.Scoped.Data.InfoTable where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Store.Scoped.Data.SymbolEntry
import Juvix.Extra.Serialize
import Juvix.Prelude

type DocTable = HashMap NameId (Judoc 'Scoped)

type PrecedenceGraph = HashMap S.NameId (HashSet S.NameId)

data InfoTable = InfoTable
  { _infoFixities :: HashMap S.NameId FixityDef,
    _infoPrecedenceGraph :: PrecedenceGraph,
    _infoConstructorSigs :: HashMap NameId (RecordNameSignature 'Scoped),
    _infoNameSigs :: HashMap NameId (NameSignature 'Scoped),
    _infoParsedConstructorSigs :: HashMap NameId (RecordNameSignature 'Parsed),
    _infoParsedNameSigs :: HashMap NameId (NameSignature 'Parsed),
    _infoRecords :: HashMap NameId RecordInfo,
    _infoFunctions :: HashMap NameId (FunctionDef 'Scoped),
    _infoInductives :: HashMap NameId (InductiveDef 'Scoped),
    _infoConstructors :: HashMap NameId (ConstructorDef 'Scoped),
    _infoAxioms :: HashMap NameId (AxiomDef 'Scoped),
    _infoBuiltins :: HashMap BuiltinPrim S.Symbol,
    _infoScoperAlias :: HashMap S.NameId PreSymbolEntry
  }
  deriving stock (Generic)

instance Serialize InfoTable

instance NFData InfoTable

makeLenses ''InfoTable

instance Semigroup InfoTable where
  tab1 <> tab2 =
    InfoTable
      { _infoFixities = tab1 ^. infoFixities <> tab2 ^. infoFixities,
        _infoPrecedenceGraph = combinePrecedenceGraphs (tab1 ^. infoPrecedenceGraph) (tab2 ^. infoPrecedenceGraph),
        _infoConstructorSigs = tab1 ^. infoConstructorSigs <> tab2 ^. infoConstructorSigs,
        _infoNameSigs = tab1 ^. infoNameSigs <> tab2 ^. infoNameSigs,
        _infoParsedConstructorSigs = tab1 ^. infoParsedConstructorSigs <> tab2 ^. infoParsedConstructorSigs,
        _infoParsedNameSigs = tab1 ^. infoParsedNameSigs <> tab2 ^. infoParsedNameSigs,
        _infoBuiltins = tab1 ^. infoBuiltins <> tab2 ^. infoBuiltins,
        _infoRecords = tab1 ^. infoRecords <> tab2 ^. infoRecords,
        _infoFunctions = tab1 ^. infoFunctions <> tab2 ^. infoFunctions,
        _infoInductives = tab1 ^. infoInductives <> tab2 ^. infoInductives,
        _infoConstructors = tab1 ^. infoConstructors <> tab2 ^. infoConstructors,
        _infoAxioms = tab1 ^. infoAxioms <> tab2 ^. infoAxioms,
        _infoScoperAlias = tab1 ^. infoScoperAlias <> tab2 ^. infoScoperAlias
      }

instance Monoid InfoTable where
  mempty =
    InfoTable
      { _infoFixities = mempty,
        _infoPrecedenceGraph = mempty,
        _infoConstructorSigs = mempty,
        _infoNameSigs = mempty,
        _infoParsedConstructorSigs = mempty,
        _infoParsedNameSigs = mempty,
        _infoRecords = mempty,
        _infoFunctions = mempty,
        _infoInductives = mempty,
        _infoConstructors = mempty,
        _infoAxioms = mempty,
        _infoBuiltins = mempty,
        _infoScoperAlias = mempty
      }

combinePrecedenceGraphs :: PrecedenceGraph -> PrecedenceGraph -> PrecedenceGraph
combinePrecedenceGraphs g1 g2 =
  HashMap.unionWith HashSet.union g1 g2
