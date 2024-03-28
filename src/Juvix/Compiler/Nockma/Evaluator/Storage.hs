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

instance (Eq a) => Eq (StorageKey a) where
  s1 == s2 = stripTags (s1 ^. storageKeyTerm) == stripTags (s2 ^. storageKeyTerm)

instance (Hashable a) => Hashable (StorageKey a) where
  hashWithSalt s k = hashWithSalt s (stripTags (k ^. storageKeyTerm))

instance (PrettyCode a, NockNatural a) => PrettyCode (StorageKey a) where
  ppCode k = ppCode (stripTags (k ^. storageKeyTerm))

emptyStorage :: (Hashable a) => Storage a
emptyStorage = Storage {_storageKeyValueData = mempty}
