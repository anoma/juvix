module Juvix.Compiler.Nockma.Evaluator.Storage where

import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty.Base
import Juvix.Prelude.Base

newtype Storage a = Storage
  {_storageKeyValueData :: HashMap (StorageKey a) (Term a)}

newtype StorageKey a = StorageKey {_storageKeyTerm :: Term a}

makeLenses ''Storage
makeLenses ''StorageKey

stripTags :: Term a -> Term a
stripTags = \case
  TermAtom a -> TermAtom (set atomTag Nothing a)
  TermCell c ->
    TermCell
      ( set cellTag Nothing
          . over cellLeft stripTags
          . over cellRight stripTags
          . over (cellCall . _Just . stdlibCallArgs) stripTags
          $ c
      )

instance (NockmaEq a) => NockmaEq (StorageKey a) where
  nockmaEq (StorageKey s1) (StorageKey s2) = nockmaEq s1 s2

instance (NockmaEq a) => Eq (StorageKey a) where
  (==) = nockmaEq

instance (NockmaEq a, Hashable a) => Hashable (StorageKey a) where
  hashWithSalt salt k = goTerm salt (k ^. storageKeyTerm)
    where
      goCell :: Int -> Cell a -> Int
      goCell s c = hashWithSalt s (c ^. cellLeft, c ^. cellRight)

      goAtom :: Int -> Atom a -> Int
      goAtom s a = hashWithSalt s (a ^. atom)

      goTerm :: Int -> Term a -> Int
      goTerm s = \case
        TermAtom a -> goAtom s a
        TermCell c -> goCell s c

instance (PrettyCode a, NockNatural a) => PrettyCode (StorageKey a) where
  ppCode k = ppCode (stripTags (k ^. storageKeyTerm))

emptyStorage :: (NockmaEq a, Hashable a) => Storage a
emptyStorage = Storage {_storageKeyValueData = mempty}
