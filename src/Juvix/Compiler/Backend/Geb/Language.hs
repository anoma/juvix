module Juvix.Compiler.Backend.Geb.Language
  ( module Juvix.Compiler.Backend.Geb.Language,
    module Juvix.Prelude,
  )
where

import Juvix.Prelude hiding (First, Product)

{-
  The following datatypes correspond to GEB types for terms
  (https://github.com/anoma/geb/blob/main/src/specs/lambda.lisp) and types
  (https://github.com/anoma/geb/blob/main/src/specs/geb.lisp).
-}

-- | Represents GEB's `case-on`. `_caseOn` is the value matched on of type
-- `_caseLeftType + _caseRightType`, `_caseLeft` has type `_caseLeftType ->
-- _caseCodomainType` and `_caseRight` has type `_caseRightType ->
-- _caseCodomainType`.
data Case = Case
  { _caseOn :: Morphism,
    _caseLeft :: Morphism,
    _caseRight :: Morphism
  }
  deriving stock (Show, Eq, Generic)

data Absurd = Absurd
  { _absurdType :: Object,
    _absurdValue :: Morphism
  }
  deriving stock (Show, Eq, Generic)

data LeftInj' a = LeftInj
  { _leftInjRightType :: Object,
    _leftInjValue :: a
  }
  deriving stock (Show, Eq, Generic)

type LeftInj = LeftInj' Morphism

data RightInj' a = RightInj
  { _rightInjLeftType :: Object,
    _rightInjValue :: a
  }
  deriving stock (Show, Eq, Generic)

type RightInj = RightInj' Morphism

data Pair' a = Pair
  { _pairLeft :: a,
    _pairRight :: a
  }
  deriving stock (Show, Eq, Generic)

type Pair = Pair' Morphism

newtype First = First
  { _firstValue :: Morphism
  }
  deriving stock (Show, Eq, Generic)

newtype Second = Second
  { _secondValue :: Morphism
  }
  deriving stock (Show, Eq, Generic)

data Lambda = Lambda
  { _lambdaVarType :: Object,
    _lambdaBody :: Morphism
  }
  deriving stock (Show, Eq, Generic)

data Application = Application
  { _applicationLeft :: Morphism,
    _applicationRight :: Morphism
  }
  deriving stock (Show, Eq, Generic)

newtype Var = Var
  { _varIndex :: Int
  }
  deriving stock (Show, Eq, Generic)

data Opcode
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpEq
  | OpLt
  deriving stock (Show, Eq, Generic)

data Binop = Binop
  { _binopOpcode :: Opcode,
    _binopLeft :: Morphism,
    _binopRight :: Morphism
  }
  deriving stock (Show, Eq, Generic)

data Failure = Failure
  { _failureMessage :: Text,
    _failureType :: Object
  }
  deriving stock (Show, Eq, Generic)

-- | Corresponds to the GEB type for terms (morphisms of the category): `stlc`
-- (https://github.com/anoma/geb/blob/main/src/specs/lambda.lisp).
data Morphism
  = MorphismAbsurd Absurd
  | MorphismUnit
  | MorphismLeft LeftInj
  | MorphismRight RightInj
  | MorphismCase Case
  | MorphismPair Pair
  | MorphismFirst First
  | MorphismSecond Second
  | MorphismLambda Lambda
  | MorphismApplication Application
  | MorphismVar Var
  | MorphismInteger Integer
  | MorphismBinop Binop
  | MorphismFail Failure
  deriving stock (Show, Eq, Generic)

data Product = Product
  { _productLeft :: Object,
    _productRight :: Object
  }
  deriving stock (Show, Eq, Generic)

data Coproduct = Coproduct
  { _coproductLeft :: Object,
    _coproductRight :: Object
  }
  deriving stock (Show, Eq, Generic)

-- | Function type
data Hom = Hom
  { _homDomain :: Object,
    _homCodomain :: Object
  }
  deriving stock (Show, Eq, Generic)

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
  deriving stock (Show, Eq, Generic)

data Expression
  = ExpressionMorphism Morphism
  | ExpressionObject Object
  | ExpressionTypedMorphism TypedMorphism
  deriving stock (Show, Eq, Generic)

data TypedMorphism = TypedMorphism
  { _typedMorphism :: Morphism,
    _typedMorphismObject :: Object
  }
  deriving stock (Show, Eq, Generic)

instance HasAtomicity Opcode where
  atomicity OpAdd = Atom
  atomicity OpSub = Atom
  atomicity OpMul = Atom
  atomicity OpDiv = Atom
  atomicity OpMod = Atom
  atomicity OpEq = Atom
  atomicity OpLt = Atom

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
    MorphismFail {} -> Aggregate appFixity

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
    ExpressionTypedMorphism tm -> atomicity tm

instance HasAtomicity TypedMorphism where
  atomicity _ = Aggregate appFixity

-- TODO: hasLoc

makeLenses ''Absurd
makeLenses ''Application
makeLenses ''Binop
makeLenses ''Case
makeLenses ''Coproduct
makeLenses ''First
makeLenses ''Failure
makeLenses ''Hom
makeLenses ''Lambda
makeLenses ''LeftInj'
makeLenses ''Morphism
makeLenses ''Object
makeLenses ''Pair'
makeLenses ''Product
makeLenses ''RightInj'
makeLenses ''Second
makeLenses ''TypedMorphism
makeLenses ''Var
