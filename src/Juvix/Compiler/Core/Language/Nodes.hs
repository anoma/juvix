-- | Polymorphic Node types
module Juvix.Compiler.Core.Language.Nodes
  ( module Juvix.Compiler.Core.Language.Base,
    module Juvix.Compiler.Core.Language.Primitives,
    module Juvix.Compiler.Core.Language.Nodes,
  )
where

import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Language.Primitives

-- | De Bruijn index of a locally bound variable.
data Var' i = Var
  { _varInfo :: i,
    _varIndex :: !Index
  }

-- | Global identifier of a function (with corresponding `Node` in the global
-- context).
data Ident' i = Ident
  { _identInfo :: i,
    _identSymbol :: !Symbol
  }

data Constant' i = Constant
  { _constantInfo :: i,
    _constantValue :: !ConstantValue
  }

data ConstantValue
  = ConstInteger !Integer
  | ConstString !Text
  deriving stock (Eq)

-- | Info about a single binder. Associated with Lambda, Pi, Let, Case or Match.
data Binder' ty = Binder
  { _binderName :: Text,
    _binderLocation :: Maybe Location,
    _binderType :: ty
  }

-- Other things we might need in the future:
-- - ConstFloat or ConstFixedPoint

data App' i a = App
  { _appInfo :: i,
    _appLeft :: !a,
    _appRight :: !a
  }

data Apps' i f a = Apps
  { _appsInfo :: i,
    _appsFun :: !f,
    _appsArgs :: ![a]
  }

-- | A builtin application. A builtin has no corresponding Node. It is treated
-- specially by the evaluator and the code generator. For example, basic
-- arithmetic operations go into `Builtin`. The number of arguments supplied
-- must be equal to the number of arguments expected by the builtin operation
-- (this simplifies evaluation and code generation). If you need partial
-- application, eta-expand with lambdas, e.g., eta-expand `(+) 2` to `\x -> (+)
-- 2 x`. See Transformation/Eta.hs.
data BuiltinApp' i a = BuiltinApp
  { _builtinAppInfo :: i,
    _builtinAppOp :: !BuiltinOp,
    _builtinAppArgs :: ![a]
  }

-- | A data constructor application. The number of arguments supplied must be
-- equal to the number of arguments expected by the constructor.
data Constr' i a = Constr
  { _constrInfo :: i,
    _constrTag :: !Tag,
    _constrArgs :: ![a]
  }

-- | Useful for unfolding lambdas
data LambdaLhs' i ty = LambdaLhs
  { _lambdaLhsInfo :: i,
    _lambdaLhsBinder :: Binder' ty
  }

data Lambda' i a ty = Lambda
  { _lambdaInfo :: i,
    _lambdaBinder :: Binder' ty,
    _lambdaBody :: !a
  }

-- | `let x := value in body` is not reducible to lambda + application for the
-- purposes of ML-polymorphic / dependent type checking or code generation!
data Let' i a ty = Let
  { _letInfo :: i,
    _letItem :: {-# UNPACK #-} !(LetItem' a ty),
    _letBody :: !a
  }

data LetItem' a ty = LetItem
  { _letItemBinder :: Binder' ty,
    _letItemValue :: a
  }

-- | Represents a block of mutually recursive local definitions. Both in the
-- body and in the values `length _letRecValues` implicit binders are introduced
-- which hold the functions/values being defined.
-- the last item in _letRecValues will have have index $0 in the body.
-- The values are *not* in scope of the binders. I.e.
-- the binders of the values cannot refer to the values
data LetRec' i a ty = LetRec
  { _letRecInfo :: i,
    _letRecValues :: !(NonEmpty (LetItem' a ty)),
    _letRecBody :: !a
  }

-- | One-level case matching on the tag of a data constructor: `Case value
-- branches default`. `Case` is lazy: only the selected branch is evaluated.
data Case' i bi a ty = Case
  { _caseInfo :: i,
    _caseInductive :: Symbol,
    _caseValue :: !a,
    _caseBranches :: ![CaseBranch' bi a ty],
    _caseDefault :: !(Maybe a)
  }

-- | `CaseBranch tag binders bindersNum branch`
-- - `binders` are the arguments of the constructor tagged with `tag`,
--  length of `binders` is equal to `bindersNum`
data CaseBranch' i a ty = CaseBranch
  { _caseBranchInfo :: i,
    _caseBranchTag :: !Tag,
    _caseBranchBinders :: [Binder' ty],
    _caseBranchBindersNum :: !Int,
    _caseBranchBody :: !a
  }

-- | A special form of `Case` for the booleans. Used only in Core.Stripped.
data If' i a = If
  { _ifInfo :: i,
    _ifValue :: !a,
    _ifTrue :: !a,
    _ifFalse :: !a
  }

-- | Complex pattern match. `Match` is lazy: only the selected branch is evaluated.
data Match' i a = Match
  { _matchInfo :: i,
    _matchValueTypes :: !(NonEmpty a),
    _matchReturnType :: !a,
    _matchValues :: !(NonEmpty a),
    _matchBranches :: ![MatchBranch' i a]
  }

-- | The patterns introduce binders from left to right, with the binder for a
-- constructor before the binders for the subpatterns, e.g., matching on the
-- patterns '(C (D x) y) u@(E _ z)' with body 'f x y z u' may be represented by
-- a 'MatchBranch' with '_matchBranchPatterns' equal to '[PatConstr Ctag
-- [PatConstr Dtag [PatWildcard], PatWildcard], PatConstr Etag [PatWildcard,
-- PatWildcard]]' and '_matchBranchBody' equal to 'App (App (App (Ident f) (Var
-- 4)) (Var 3)) (Var 0) (Var 2)'. So the de Bruijn indices increase from right
-- to left.
data MatchBranch' i a = MatchBranch
  { _matchBranchInfo :: i,
    _matchBranchPatterns :: !(NonEmpty (Pattern' i a)),
    _matchBranchBody :: !a
  }

data Pattern' i a
  = PatWildcard (PatternWildcard' i a)
  | PatConstr (PatternConstr' i a)

data PatternWildcard' i a = PatternWildcard
  { _patternWildcardInfo :: i,
    _patternWildcardBinder :: Binder' a
  }

data PatternConstr' i a = PatternConstr
  { _patternConstrInfo :: i,
    _patternConstrBinder :: Binder' a,
    _patternConstrTag :: !Tag,
    _patternConstrArgs :: ![Pattern' i a]
  }

-- | Useful for unfolding Pi
data PiLhs' i a = PiLhs
  { _piLhsInfo :: i,
    _piLhsBinder :: Binder' a
  }

-- | Dependent Pi-type. Compilation-time only. Pi implicitly introduces a binder
-- in the body, exactly like Lambda. So `Pi info ty body` is `Pi x : ty .
-- body` in more familiar notation, but references to `x` in `body` are via de
-- Bruijn index. For example, Pi A : Type . A -> A translates to (omitting
-- Infos): Pi (Univ level) (Pi (Var 0) (Var 1)).
data Pi' i a = Pi
  { _piInfo :: i,
    _piBinder :: Binder' a,
    _piBody :: !a
  }

-- | Universe. Compilation-time only.
data Univ' i = Univ
  { _univInfo :: i,
    _univLevel :: !Int
  }

-- | Type constructor application. Compilation-time only.
data TypeConstr' i a = TypeConstr
  { _typeConstrInfo :: i,
    _typeConstrSymbol :: !Symbol,
    _typeConstrArgs :: ![a]
  }

-- | A primitive type.
data TypePrim' i = TypePrim
  { _typePrimInfo :: i,
    _typePrimPrimitive :: Primitive
  }

-- | Dynamic type. A Node with a dynamic type has an unknown type. Useful
-- for transformations that introduce partial type information, e.g., one can
-- have types `* -> *` and `* -> * -> Nat` where `*` is the dynamic type.
newtype Dynamic' i = Dynamic
  { _dynamicInfo :: i
  }

-- | A fail node.
newtype Bottom' i = Bottom
  { _bottomInfo :: i
  }

{-------------------------------------------------------------------}
{- Typeclass instances -}

instance HasAtomicity (Var' i) where
  atomicity _ = Atom

instance HasAtomicity (Ident' i) where
  atomicity _ = Atom

instance HasAtomicity (Constant' i) where
  atomicity _ = Atom

instance HasAtomicity (App' i a) where
  atomicity _ = Aggregate appFixity

instance HasAtomicity (Apps' f i a) where
  atomicity _ = Aggregate appFixity

instance HasAtomicity (BuiltinApp' i a) where
  atomicity BuiltinApp {..}
    | null _builtinAppArgs = Atom
    | otherwise = Aggregate lambdaFixity

instance HasAtomicity (Constr' i a) where
  atomicity Constr {..}
    | null _constrArgs = Atom
    | otherwise = Aggregate lambdaFixity

instance HasAtomicity (Lambda' i a ty) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Let' i a ty) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (LetRec' i a ty) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Case' i bi a ty) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (If' i a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Match' i a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (PatternWildcard' i a) where
  atomicity _ = Atom

instance HasAtomicity (PatternConstr' i a) where
  atomicity PatternConstr {..}
    | null _patternConstrArgs = Atom
    | otherwise = Aggregate appFixity

instance HasAtomicity (Pattern' i a) where
  atomicity = \case
    PatWildcard x -> atomicity x
    PatConstr x -> atomicity x

instance HasAtomicity (Pi' i a) where
  atomicity _ = Aggregate funFixity

instance HasAtomicity (Univ' i) where
  atomicity _ = Atom

instance HasAtomicity (TypePrim' i) where
  atomicity _ = Atom

instance HasAtomicity (TypeConstr' i a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Dynamic' i) where
  atomicity _ = Atom

instance HasAtomicity (Bottom' i) where
  atomicity _ = Atom

lambdaFixity :: Fixity
lambdaFixity = Fixity (PrecNat 0) (Unary AssocPostfix)

makeLenses ''LambdaLhs'
makeLenses ''PiLhs'
makeLenses ''Binder'
makeLenses ''Var'
makeLenses ''Ident'
makeLenses ''Constant'
makeLenses ''App'
makeLenses ''BuiltinApp'
makeLenses ''Constr'
makeLenses ''Let'
makeLenses ''LetRec'
makeLenses ''Case'
makeLenses ''CaseBranch'
makeLenses ''Match'
makeLenses ''MatchBranch'
makeLenses ''PatternWildcard'
makeLenses ''PatternConstr'
makeLenses ''Pi'
makeLenses ''Lambda'
makeLenses ''Univ'
makeLenses ''TypeConstr'
makeLenses ''Dynamic'
makeLenses ''LetItem'

instance Eq ty => Eq (Binder' ty) where
  (==) = eqOn (^. binderType)

instance Eq (Var' i) where
  (Var _ idx1) == (Var _ idx2) = idx1 == idx2

instance Ord (Var' i) where
  compare = compare `on` (^. varIndex)

instance Eq (Ident' i) where
  (Ident _ sym1) == (Ident _ sym2) = sym1 == sym2

instance Eq (Constant' i) where
  (Constant _ v1) == (Constant _ v2) = v1 == v2

instance (Eq a) => Eq (App' i a) where
  (App _ l1 r1) == (App _ l2 r2) = l1 == l2 && r1 == r2

instance (Eq f, Eq a) => Eq (Apps' i f a) where
  (Apps _ op1 args1) == (Apps _ op2 args2) = op1 == op2 && args1 == args2

instance (Eq a) => Eq (BuiltinApp' i a) where
  (BuiltinApp _ op1 args1) == (BuiltinApp _ op2 args2) = op1 == op2 && args1 == args2

instance (Eq a) => Eq (Constr' i a) where
  (Constr _ tag1 args1) == (Constr _ tag2 args2) = tag1 == tag2 && args1 == args2

instance (Eq a) => Eq (Case' i bi a ty) where
  (Case _ sym1 v1 bs1 def1) == (Case _ sym2 v2 bs2 def2) = sym1 == sym2 && v1 == v2 && bs1 == bs2 && def1 == def2

instance (Eq a) => Eq (If' i a) where
  (If _ v1 b1 c1) == (If _ v2 b2 c2) = v1 == v2 && b1 == b2 && c1 == c2

instance (Eq a) => Eq (CaseBranch' i a ty) where
  (==) =
    eqOn (^. caseBranchTag)
      ..&&.. eqOn (^. caseBranchBody)

instance (Eq a) => Eq (Match' i a) where
  (Match _ _ _ vs1 bs1) == (Match _ _ _ vs2 bs2) = vs1 == vs2 && bs1 == bs2

instance Eq (PatternWildcard' i a) where
  _ == _ = True

instance Eq (Univ' i) where
  (Univ _ l1) == (Univ _ l2) = l1 == l2

instance (Eq a) => Eq (TypeConstr' i a) where
  (TypeConstr _ sym1 args1) == (TypeConstr _ sym2 args2) = sym1 == sym2 && args1 == args2

instance Eq (TypePrim' i) where
  (TypePrim _ p1) == (TypePrim _ p2) = p1 == p2

instance Eq (Dynamic' i) where
  (Dynamic _) == (Dynamic _) = True

instance Eq (Bottom' i) where
  (Bottom _) == (Bottom _) = True

deriving stock instance (Eq a) => Eq (Pattern' i a)

instance (Eq a, Eq ty) => Eq (LetItem' a ty) where
  (==) = eqOn (^. letItemValue) ..&&.. eqOn (^. letItemBinder)

instance (Eq a, Eq ty) => Eq (Lambda' i a ty) where
  (Lambda _ bd1 body1) == (Lambda _ bd2 body2) = bd1 == bd2 && body1 == body2

instance (Eq a, Eq ty) => Eq (Let' i a ty) where
  (==) =
    eqOn (^. letItem)
      ..&&.. eqOn (^. letBody)

instance (Eq a, Eq ty) => Eq (LetRec' i a ty) where
  (==) =
    eqOn (^. letRecBody)
      ..&&.. eqOn (^. letRecValues)

instance (Eq a) => Eq (Pi' i a) where
  (==) =
    eqOn (^. piBinder . binderType)
      ..&&.. eqOn (^. piBody)

instance (Eq a) => Eq (MatchBranch' i a) where
  (MatchBranch _ pats1 b1) == (MatchBranch _ pats2 b2) = pats1 == pats2 && b1 == b2

instance (Eq a) => Eq (PatternConstr' i a) where
  (PatternConstr _ _ tag1 ps1) == (PatternConstr _ _ tag2 ps2) = tag1 == tag2 && ps1 == ps2

instance Hashable (Ident' i) where
  hashWithSalt s = hashWithSalt s . (^. identSymbol)

instance Hashable (Var' i) where
  hashWithSalt s = hashWithSalt s . (^. varIndex)
