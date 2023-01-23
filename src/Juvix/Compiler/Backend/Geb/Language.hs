module Juvix.Compiler.Backend.Geb.Language where

import Juvix.Prelude

{-
  The following datatypes correspond to GEB types for terms
  (https://github.com/anoma/geb/blob/main/src/specs/lambda.lisp) and types
  (https://github.com/anoma/geb/blob/main/src/specs/geb.lisp).
-}

data Case = Case
  { _caseLeftType :: Obj,
    _caseRightType :: Obj,
    _caseCodomainType :: Obj,
    _caseOn :: Geb,
    _caseLeft :: Geb,
    _caseRight :: Geb
  }

data Pair = Pair
  { _pairLeftType :: Obj,
    _pairRightType :: Obj,
    _pairLeft :: Geb,
    _pairRight :: Geb
  }

data Fst = Fst
  { _fstLeftType :: Obj,
    _fstRightType :: Obj,
    _fstValue :: Geb
  }

data Snd = Snd
  { _sndLeftType :: Obj,
    _sndRightType :: Obj,
    _sndValue :: Geb
  }

data Lamb = Lamb
  { _lambVarType :: Obj,
    _lambBodyType :: Obj,
    _lambBody :: Geb
  }

data App = App
  { _appDomainType :: Obj,
    _appCodomainType :: Obj,
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
  { _prodLeft :: Obj,
    _prodRight :: Obj
  }

data Coprod = Coprod
  { _coprodLeft :: Obj,
    _coprodRight :: Obj
  }

-- | Function type
data Hom = Hom
  { _homDomain :: Obj,
    _homCodomain :: Obj
  }

-- | Corresponds to the GEB type for types (objects of the category): `substobj`
-- (https://github.com/anoma/geb/blob/main/src/specs/geb.lisp).
data Obj
  = ObjInitial -- the empty type
  | ObjTerminal -- the unit type
  | ObjProd Prod
  | ObjCoprod Coprod
  | ObjHom Hom

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

instance HasAtomicity Obj where
  atomicity = \case
    ObjInitial -> Atom
    ObjTerminal -> Atom
    ObjProd {} -> Aggregate appFixity
    ObjCoprod {} -> Aggregate appFixity
    ObjHom {} -> Aggregate appFixity
