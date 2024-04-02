module Juvix.Compiler.Nockma.Evaluator.Storage where

import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty.Base
import Juvix.Prelude.Base

newtype Storage a = Storage
  {_storageKeyValueData :: HashMap (StorageKey a) (Term a)}

newtype StorageKey a = StorageKey {_storageKeyTerm :: Term a}

makeLenses ''Storage
makeLenses ''StorageKey

stripMeta :: Term a -> Term a
stripMeta = \case
  TermAtom a ->
    TermAtom
      Atom
        { _atom = a ^. atom,
          _atomInfo = emptyAtomInfo
        }
  TermCell c ->
    TermCell
      Cell'
        { _cellLeft = stripMeta (c ^. cellLeft),
          _cellRight = stripMeta (c ^. cellRight),
          _cellInfo = emptyCellInfo
        }

instance (NockmaEq a) => NockmaEq (StorageKey a) where
  nockmaEq (StorageKey s1) (StorageKey s2) = nockmaEq s1 s2

instance (NockmaEq a) => Eq (StorageKey a) where
  (==) = nockmaEq

instance (NockmaEq a, Hashable a) => Hashable (StorageKey a) where
  hashWithSalt salt k = goTerm (k ^. storageKeyTerm)
    where
      goCell :: Cell a -> Int
      goCell c = hashWithSalt salt (c ^. cellLeft, c ^. cellRight)

      goAtom :: Atom a -> Int
      goAtom a = hashWithSalt salt (a ^. atom)

      goTerm :: Term a -> Int
      goTerm = \case
        TermAtom a -> goAtom a
        TermCell c -> goCell c

instance (PrettyCode a, NockNatural a) => PrettyCode (StorageKey a) where
  ppCode k = ppCode (stripMeta (k ^. storageKeyTerm))

emptyStorage :: (NockmaEq a, Hashable a) => Storage a
emptyStorage = Storage {_storageKeyValueData = mempty}
