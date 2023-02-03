module Juvix.Compiler.Backend.Geb.Language
  ( module Juvix.Compiler.Backend.Geb.Language,
    module Juvix.Prelude,
  )
where

import Juvix.Prelude hiding (First, Product)

data Expression = ExpressionMorphism Morphism | ExpressionObject Object

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
    _caseOn :: Morphism,
    _caseLeft :: Morphism,
    _caseRight :: Morphism
  }
  deriving stock (Show, Eq)

data Pair = Pair
  { _pairLeftType :: Object,
    _pairRightType :: Object,
    _pairLeft :: Morphism,
    _pairRight :: Morphism
  }
  deriving stock (Show, Eq)

data First = First
  { _firstLeftType :: Object,
    _firstRightType :: Object,
    _firstValue :: Morphism
  }
  deriving stock (Show, Eq)

data Second = Second
  { _secondLeftType :: Object,
    _secondRightType :: Object,
    _secondValue :: Morphism
  }
  deriving stock (Show, Eq)

data Lambda = Lambda
  { _lambdaVarType :: Object,
    _lambdaBodyType :: Object,
    _lambdaBody :: Morphism
  }
  deriving stock (Show, Eq)

data Application = Application
  { _applicationDomainType :: Object,
    _applicationCodomainType :: Object,
    _applicationLeft :: Morphism,
    _applicationRight :: Morphism
  }

newtype Var = Var {_varIndex :: Int}

data Opcode
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpEq
  | OpLt
  deriving stock (Show, Eq)

data Binop = Binop
  { _binopOpcode :: Opcode,
    _binopLeft :: Morphism,
    _binopRight :: Morphism
  }
  deriving stock (Show, Eq)

-- | Corresponds to the GEB type for terms (morphisms of the category): `stlc`
-- (https://github.com/anoma/geb/blob/main/src/specs/lambda.lisp).
data Morphism
  = MorphismAbsurd Morphism
  | MorphismUnit
  | MorphismLeft Morphism
  | MorphismRight Morphism
  | MorphismCase Case
  | MorphismPair Pair
  | MorphismFirst First
  | MorphismSecond Second
  | MorphismLambda Lambda
  | MorphismApplication Application
  | MorphismVar Var
  | MorphismInteger Integer
  | MorphismBinop Binop
  deriving stock (Show, Eq)

data Product = Product
  { _productLeft :: Object,
    _productRight :: Object
  }
  deriving stock (Show, Eq)

data Coproduct = Coproduct
  { _coproductLeft :: Object,
    _coproductRight :: Object
  }
  deriving stock (Show, Eq)

-- | Function type
data Hom = Hom
  { _homDomain :: Object,
    _homCodomain :: Object
  }
  deriving stock (Show, Eq)

-- | Corresponds to the GEB type for types (objects of the category): `substobj`
-- (https://github.com/anoma/geb/blob/main/src/specs/geb.lisp).
data Object
  = -- | empty type
    ObjectInitial
  | -- | unit type
    ObjectTerminal
  | ObjectProduct Product
  | ObjectCoproduct Coproduct
  | -- | function type
    ObjectHom Hom
  | ObjectInteger
  deriving stock (Show, Eq)

instance HasAtomicity Morphism where
  atomicity = \case
    MorphismAbsurd {} -> Aggregate appFixity
    MorphismUnit -> Atom
    MorphismLeft {} -> Aggregate appFixity
    MorphismRight {} -> Aggregate appFixity
    MorphismCase {} -> Aggregate appFixity
    MorphismPair {} -> Aggregate appFixity
    MorphismFirst {} -> Aggregate appFixity
    MorphismSecond {} -> Aggregate appFixity
    MorphismLambda {} -> Aggregate appFixity
    MorphismApplication {} -> Aggregate appFixity
    MorphismVar {} -> Aggregate appFixity
    MorphismInteger {} -> Atom
    MorphismBinop {} -> Aggregate appFixity

instance HasAtomicity Object where
  atomicity = \case
    ObjectInitial -> Atom
    ObjectTerminal -> Atom
    ObjectProduct {} -> Aggregate appFixity
    ObjectCoproduct {} -> Aggregate appFixity
    ObjectHom {} -> Aggregate appFixity
    ObjectInteger -> Atom

instance HasAtomicity Expression where
  atomicity = \case
    ExpressionMorphism m -> atomicity m
    ExpressionObject o -> atomicity o

makeLenses ''Case
makeLenses ''Pair
makeLenses ''First
makeLenses ''Second
makeLenses ''Lambda
makeLenses ''Var
makeLenses ''Binop
makeLenses ''Application
makeLenses ''Morphism
makeLenses ''Product
makeLenses ''Coproduct
makeLenses ''Hom
makeLenses ''Object
