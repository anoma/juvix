module Juvix.Compiler.Backend.Geb.Language where

import Juvix.Prelude

{-
  The following datatypes correspond to GEB types for terms
  (https://github.com/anoma/geb/blob/main/src/specs/lambda.lisp) and types
  (https://github.com/anoma/geb/blob/main/src/specs/geb.lisp).
-}

data Case = Case
  { _caseLeftType :: Type,
    _caseRightType :: Type,
    _caseCodomainType :: Type,
    _caseOn :: Geb,
    _caseLeft :: Geb,
    _caseRight :: Geb
  }

data Pair = Pair
  { _pairLeftType :: Type,
    _pairRightType :: Type,
    _pairLeft :: Geb,
    _pairRight :: Geb
  }

data Fst = Fst
  { _fstLeftType :: Type,
    _fstRightType :: Type,
    _fstValue :: Geb
  }

data Snd = Snd
  { _sndLeftType :: Type,
    _sndRightType :: Type,
    _sndValue :: Geb
  }

data Lamb = Lamb
  { _lambVarType :: Type,
    _lambBodyType :: Type,
    _lambBody :: Geb
  }

data App = App
  { _appDomainType :: Type,
    _appCodomainType :: Type,
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
  { _prodLeft :: Type,
    _prodRight :: Type
  }

data Coprod = Coprod
  { _coprodLeft :: Type,
    _coprodRight :: Type
  }

-- | Corresponds to a subset of the GEB type `substobj`
-- (https://github.com/anoma/geb/blob/main/src/specs/geb.lisp).
data Type
  = TypeInitial -- the empty type
  | TypeTerminal -- the unit type
  | TypeProd Prod
  | TypeCoprod Coprod

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

instance HasAtomicity Type where
  atomicity = \case
    TypeInitial -> Atom
    TypeTerminal -> Atom
    TypeProd {} -> Aggregate appFixity
    TypeCoprod {} -> Aggregate appFixity
