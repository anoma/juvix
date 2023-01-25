module Juvix.Compiler.Backend.Geb.Language where

import Juvix.Prelude

{-
  The following datatypes correspond to GEB types for terms
  (https://github.com/anoma/geb/blob/main/src/specs/lambda.lisp) and types
  (https://github.com/anoma/geb/blob/main/src/specs/geb.lisp).
-}

-- | Represents GEB's `case-on`. `_caseOn` is the value matched on of type
-- `Dom`, `_caseLeft` has type `_caseLeftType -> _caseCodomainType` and
-- `_caseRight` has type `_caseRightType -> _caseCodomainType`.
data Case = Case
  { _caseLeftType :: Object,
    _caseRightType :: Object,
    _caseCodomainType :: Object,
    _caseOn :: Geb,
    _caseLeft :: Geb,
    _caseRight :: Geb
  }

data Pair = Pair
  { _pairLeftType :: Object,
    _pairRightType :: Object,
    _pairLeft :: Geb,
    _pairRight :: Geb
  }

data Fst = Fst
  { _fstLeftType :: Object,
    _fstRightType :: Object,
    _fstValue :: Geb
  }

data Snd = Snd
  { _sndLeftType :: Object,
    _sndRightType :: Object,
    _sndValue :: Geb
  }

data Lamb = Lamb
  { _lambVarType :: Object,
    _lambBodyType :: Object,
    _lambBody :: Geb
  }

data App = App
  { _appDomainType :: Object,
    _appCodomainType :: Object,
    _appLeft :: Geb,
    _appRight :: Geb
  }

-- | Corresponds to the GEB type for terms: `stlc`
-- (https://github.com/anoma/geb/blob/main/src/specs/lambda.lisp).
data Geb
  = GebAbsurd Geb
  | GebUnit
  | GebLeft Geb
  | GebRight Geb
  | GebCase Case
  | GebPair Pair
  | GebFst Fst
  | GebSnd Snd
  | GebLamb Lamb
  | GebApp App
  | GebVar Int

data Prod = Prod
  { _prodLeft :: Object,
    _prodRight :: Object
  }

data Coprod = Coprod
  { _coprodLeft :: Object,
    _coprodRight :: Object
  }

-- | Function type
data Hom = Hom
  { _homDomain :: Object,
    _homCodomain :: Object
  }

-- | Corresponds to the GEB type for types (objects of the category): `substobj`
-- (https://github.com/anoma/geb/blob/main/src/specs/geb.lisp).
data Object
  = ObjectInitial -- empty type
  | ObjectTerminal -- unit type
  | ObjectProd Prod
  | ObjectCoprod Coprod
  | ObjectHom Hom -- function type

instance HasAtomicity Geb where
  atomicity = \case
    GebAbsurd {} -> Aggregate appFixity
    GebUnit -> Atom
    GebLeft {} -> Aggregate appFixity
    GebRight {} -> Aggregate appFixity
    GebCase {} -> Aggregate appFixity
    GebPair {} -> Aggregate appFixity
    GebFst {} -> Aggregate appFixity
    GebSnd {} -> Aggregate appFixity
    GebLamb {} -> Aggregate appFixity
    GebApp {} -> Aggregate appFixity
    GebVar {} -> Aggregate appFixity

instance HasAtomicity Object where
  atomicity = \case
    ObjectInitial -> Atom
    ObjectTerminal -> Atom
    ObjectProd {} -> Aggregate appFixity
    ObjectCoprod {} -> Aggregate appFixity
    ObjectHom {} -> Aggregate appFixity

makeLenses ''Case
makeLenses ''Pair
makeLenses ''Fst
makeLenses ''Snd
makeLenses ''Lamb
makeLenses ''App
makeLenses ''Geb
makeLenses ''Prod
makeLenses ''Coprod
makeLenses ''Hom
makeLenses ''Object
