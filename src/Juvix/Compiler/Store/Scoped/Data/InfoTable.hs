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

type BuiltinsTable = HashMap BuiltinPrim S.Symbol

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
    -- | Contains the builtins defined in itself *and* its local modules
    _infoBuiltins :: BuiltinsTable,
    _infoScoperAlias :: HashMap S.NameId PreSymbolEntry
  }
  deriving stock (Generic)

instance Serialize InfoTable

instance NFData InfoTable

makeLenses ''InfoTable

instance Semigroup InfoTable where
  tab1 <> tab2 =
    InfoTable
      { _infoFixities = mappendField' infoFixities,
        _infoPrecedenceGraph = appendFieldWith' combinePrecedenceGraphs infoPrecedenceGraph,
        _infoConstructorSigs = mappendField' infoConstructorSigs,
        _infoNameSigs = mappendField' infoNameSigs,
        _infoParsedConstructorSigs = mappendField' infoParsedConstructorSigs,
        _infoParsedNameSigs = mappendField' infoParsedNameSigs,
        _infoBuiltins = mappendField' infoBuiltins,
        _infoRecords = mappendField' infoRecords,
        _infoFunctions = mappendField' infoFunctions,
        _infoInductives = mappendField' infoInductives,
        _infoConstructors = mappendField' infoConstructors,
        _infoAxioms = mappendField' infoAxioms,
        _infoScoperAlias = mappendField' infoScoperAlias
      }
    where
      mappendField' :: (Semigroup f) => Lens' InfoTable f -> f
      mappendField' = appendFieldWith' (<>)

      appendFieldWith' :: (f -> f -> f) -> Lens' InfoTable f -> f
      appendFieldWith' = appendFieldWith tab1 tab2

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

filterByTopModule :: ModuleId -> HashMap NameId b -> HashMap NameId b
filterByTopModule m = HashMap.filterWithKey (\k _v -> sameModule k)
  where
    sameModule :: NameId -> Bool
    sameModule n = m == n ^. nameIdModuleId
