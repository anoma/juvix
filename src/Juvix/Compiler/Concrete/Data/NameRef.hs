{-# LANGUAGE UndecidableInstances #-}

module Juvix.Compiler.Concrete.Data.NameRef where

import Data.Kind qualified as GHC
import Juvix.Compiler.Concrete.Data.Name qualified as C
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Prelude hiding (show)
import Prelude (show)

type RefNameType :: S.IsConcrete -> GHC.Type
type family RefNameType c = res | res -> c where
  RefNameType 'S.Concrete = S.Name
  RefNameType 'S.NotConcrete = S.Name' ()

type AxiomRef = AxiomRef' 'S.Concrete

newtype AxiomRef' (n :: S.IsConcrete) = AxiomRef'
  {_axiomRefName :: RefNameType n}

makeLenses ''AxiomRef'

instance HasLoc AxiomRef where
  getLoc = getLoc . (^. axiomRefName)

instance Hashable (RefNameType s) => Hashable (AxiomRef' s) where
  hashWithSalt i = hashWithSalt i . (^. axiomRefName)

instance (Eq (RefNameType s)) => Eq (AxiomRef' s) where
  (==) = (==) `on` (^. axiomRefName)

instance (Ord (RefNameType s)) => Ord (AxiomRef' s) where
  compare = compare `on` (^. axiomRefName)

instance (Show (RefNameType s)) => Show (AxiomRef' s) where
  show = show . (^. axiomRefName)

type InductiveRef = InductiveRef' 'S.Concrete

newtype InductiveRef' (n :: S.IsConcrete) = InductiveRef'
  { _inductiveRefName :: RefNameType n
  }

makeLenses ''InductiveRef'

instance HasLoc InductiveRef where
  getLoc = getLoc . (^. inductiveRefName)

instance Hashable (RefNameType s) => Hashable (InductiveRef' s) where
  hashWithSalt i = hashWithSalt i . (^. inductiveRefName)

instance (Eq (RefNameType s)) => Eq (InductiveRef' s) where
  (==) = (==) `on` (^. inductiveRefName)

instance (Ord (RefNameType s)) => Ord (InductiveRef' s) where
  compare = compare `on` (^. inductiveRefName)

instance (Show (RefNameType s)) => Show (InductiveRef' s) where
  show = show . (^. inductiveRefName)

type FunctionRef = FunctionRef' 'S.Concrete

newtype FunctionRef' (n :: S.IsConcrete) = FunctionRef'
  { _functionRefName :: RefNameType n
  }

makeLenses ''FunctionRef'

instance HasLoc FunctionRef where
  getLoc = getLoc . (^. functionRefName)

instance Hashable (RefNameType s) => Hashable (FunctionRef' s) where
  hashWithSalt i = hashWithSalt i . (^. functionRefName)

instance (Eq (RefNameType s)) => Eq (FunctionRef' s) where
  (==) = (==) `on` (^. functionRefName)

instance (Ord (RefNameType s)) => Ord (FunctionRef' s) where
  compare = compare `on` (^. functionRefName)

instance (Show (RefNameType s)) => Show (FunctionRef' s) where
  show = show . (^. functionRefName)

type ConstructorRef = ConstructorRef' 'S.Concrete

newtype ConstructorRef' (n :: S.IsConcrete) = ConstructorRef'
  { _constructorRefName :: RefNameType n
  }

makeLenses ''ConstructorRef'

instance HasLoc ConstructorRef where
  getLoc = getLoc . (^. constructorRefName)

instance Hashable (RefNameType s) => Hashable (ConstructorRef' s) where
  hashWithSalt i = hashWithSalt i . (^. constructorRefName)

instance (Eq (RefNameType s)) => Eq (ConstructorRef' s) where
  (==) = (==) `on` (^. constructorRefName)

instance (Ord (RefNameType s)) => Ord (ConstructorRef' s) where
  compare = compare `on` (^. constructorRefName)

instance (Show (RefNameType s)) => Show (ConstructorRef' s) where
  show = show . (^. constructorRefName)

concreteFunctionRef :: C.Name -> FunctionRef' 'S.NotConcrete -> FunctionRef
concreteFunctionRef s = over functionRefName (set S.nameConcrete s)
