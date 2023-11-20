module Juvix.Compiler.Store.Scoped.Data.InfoTable where

import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Extra.Serialize
import Juvix.Prelude

type DocTable = HashMap NameId (Judoc 'Scoped)

data InfoTable = InfoTable
  { _infoFixities :: HashMap S.NameId FixityDef,
    _infoPrecedenceGraph :: HashMap S.NameId (HashSet S.NameId),
    _infoHighlightDoc :: DocTable,
    _infoHighlightNames :: [S.AName],
    _infoConstructorSigs :: HashMap NameId (RecordNameSignature 'Scoped),
    _infoNameSigs :: HashMap NameId (NameSignature 'Scoped),
    _infoParsedConstructorSigs :: HashMap NameId (RecordNameSignature 'Parsed),
    _infoParsedNameSigs :: HashMap NameId (NameSignature 'Parsed)
  }
  deriving stock (Generic)

instance Serialize InfoTable

makeLenses ''InfoTable

instance Semigroup InfoTable where
  tab1 <> tab2 =
    InfoTable
      { _infoFixities = tab1 ^. infoFixities <> tab2 ^. infoFixities,
        _infoPrecedenceGraph = tab1 ^. infoPrecedenceGraph <> tab2 ^. infoPrecedenceGraph, -- TODO: compute precedence graphs
        _infoHighlightDoc = tab1 ^. infoHighlightDoc <> tab2 ^. infoHighlightDoc,
        _infoHighlightNames = tab1 ^. infoHighlightNames <> tab2 ^. infoHighlightNames,
        _infoConstructorSigs = tab1 ^. infoConstructorSigs <> tab2 ^. infoConstructorSigs,
        _infoNameSigs = tab1 ^. infoNameSigs <> tab2 ^. infoNameSigs,
        _infoParsedConstructorSigs = tab1 ^. infoParsedConstructorSigs <> tab2 ^. infoParsedConstructorSigs,
        _infoParsedNameSigs = tab1 ^. infoParsedNameSigs <> tab2 ^. infoParsedNameSigs
      }

instance Monoid InfoTable where
  mempty =
    InfoTable
      { _infoFixities = mempty,
        _infoPrecedenceGraph = mempty,
        _infoHighlightDoc = mempty,
        _infoHighlightNames = mempty,
        _infoConstructorSigs = mempty,
        _infoNameSigs = mempty,
        _infoParsedConstructorSigs = mempty,
        _infoParsedNameSigs = mempty
      }
