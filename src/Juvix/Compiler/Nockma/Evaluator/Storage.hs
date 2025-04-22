module Juvix.Compiler.Nockma.Evaluator.Storage where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Nockma.Data.Module
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty.Base
import Juvix.Prelude hiding (Atom)
import Juvix.Prelude.Bytes

newtype Storage a = Storage
  {_storageKeyValueData :: HashMap (StorageKey a) (Term a)}
  deriving stock (Generic)

newtype StorageKey a = StorageKey {_storageKeyTerm :: Term a}
  deriving stock (Generic)

instance (NFData a) => NFData (StorageKey a)

instance (NFData a) => NFData (Storage a)

makeLenses ''Storage
makeLenses ''StorageKey

stripMeta :: (Hashable a) => Term a -> Term a
stripMeta = \case
  TermAtom a ->
    TermAtom
      Atom
        { _atom = a ^. atom,
          _atomInfo = emptyAtomInfo
        }
  TermCell c ->
    TermCell $
      mkCell (stripMeta (c ^. cellLeft)) (stripMeta (c ^. cellRight))

instance (Hashable a, NockmaEq a) => NockmaEq (StorageKey a) where
  nockmaEq (StorageKey s1) (StorageKey s2) = nockmaEq s1 s2

instance (Hashable a, NockmaEq a) => Eq (StorageKey a) where
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

instance (PrettyCode a, Hashable a, NockNatural a) => PrettyCode (StorageKey a) where
  ppCode k = ppCode (stripMeta (k ^. storageKeyTerm))

instance (Hashable a, NockmaEq a) => Semigroup (Storage a) where
  Storage s1 <> Storage s2 =
    Storage
      { _storageKeyValueData = HashMap.union s1 s2
      }

instance (NockmaEq a, Hashable a) => Monoid (Storage a) where
  mempty = emptyStorage

emptyStorage :: (NockmaEq a, Hashable a) => Storage a
emptyStorage = Storage {_storageKeyValueData = mempty}

mkModuleStorage :: ModuleTable -> Storage Natural
mkModuleStorage mtab =
  Storage
    { _storageKeyValueData =
        HashMap.fromList (map mkStorageElem (HashMap.elems (mtab ^. moduleTable)))
    }
  where
    mkStorageElem :: Module -> (StorageKey Natural, Term Natural)
    mkStorageElem md' =
      ( StorageKey
          { _storageKeyTerm =
              TermAtom
                . mkEmptyAtom
                . byteStringToNatural
                . fromJust
                $ (md' ^. moduleInfoTable . infoSHA256)
          },
        fromJust (md' ^. moduleInfoTable . infoCode)
      )

insertJammedStorage :: (Member (Error SimpleError) r) => ByteString -> Storage Natural -> Sem r (Storage Natural)
insertJammedStorage jammedTerm storage = do
  term <- decodeCue jammedTerm
  return $ over storageKeyValueData (HashMap.insert (StorageKey sha256) term) storage
  where
    sha256 =
      TermAtom
        . byteStringToAtom'
        . SHA256.hash
        $ jammedTerm
