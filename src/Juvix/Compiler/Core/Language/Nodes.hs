module Juvix.Compiler.Core.Language.Nodes where

import Juvix.Compiler.Core.Language.Base

{-------------------------------------------------------------------}
{- Polymorphic Node types -}

-- | De Bruijn index of a locally bound variable.
data Var' i = Var {_varInfo :: i, _varIndex :: !Index}

-- | Global identifier of a function (with corresponding `Node` in the global
-- context).
data Ident' i = Ident {_identInfo :: i, _identSymbol :: !Symbol}

data Constant' i = Constant {_constantInfo :: i, _constantValue :: !ConstantValue}

data ConstantValue
  = ConstInteger !Integer
  | ConstString !Text
  deriving stock (Eq)

-- Other things we might need in the future:
-- - ConstFloat or ConstFixedPoint

data App' i a = App {_appInfo :: i, _appLeft :: !a, _appRight :: !a}

data Apps' f i a = Apps {_appsInfo :: i, _appsFun :: !f, _appsArgs :: ![a]}

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

data Lambda' i a = Lambda {_lambdaInfo :: i, _lambdaBody :: !a}

-- | `let x := value in body` is not reducible to lambda + application for the
-- purposes of ML-polymorphic / dependent type checking or code generation!
data Let' i a = Let {_letInfo :: i, _letValue :: !a, _letBody :: !a}

-- | Represents a block of mutually recursive local definitions. Both in the
-- body and in the values `length _letRecValues` implicit binders are introduced
-- which hold the functions/values being defined.
data LetRec' i a = LetRec
  { _letRecInfo :: i,
    _letRecValues :: !(NonEmpty a),
    _letRecBody :: !a
  }

-- | One-level case matching on the tag of a data constructor: `Case value
-- branches default`. `Case` is lazy: only the selected branch is evaluated.
data Case' i bi a = Case
  { _caseInfo :: i,
    _caseValue :: !a,
    _caseBranches :: ![CaseBranch' bi a],
    _caseDefault :: !(Maybe a)
  }

-- | `CaseBranch tag argsNum branch`
-- - `argsNum` is the number of arguments of the constructor tagged with `tag`,
--   equal to the number of implicit binders above `branch`
data CaseBranch' i a = CaseBranch
  { _caseBranchInfo :: i,
    _caseBranchTag :: !Tag,
    _caseBranchBindersNum :: !Int,
    _caseBranchBody :: !a
  }

-- | Dependent Pi-type. Compilation-time only. Pi implicitly introduces a binder
-- in the body, exactly like Lambda. So `Pi info ty body` is `Pi x : ty .
-- body` in more familiar notation, but references to `x` in `body` are via de
-- Bruijn index. For example, Pi A : GHC.Type . A -> A translates to (omitting
-- Infos): Pi (Univ level) (Pi (Var 0) (Var 1)).
data Pi' i a = Pi {_piInfo :: i, _piType :: !a, _piBody :: !a}

-- | Universe. Compilation-time only.
data Univ' i = Univ {_univInfo :: i, _univLevel :: !Int}

-- | GHC.Type constructor application. Compilation-time only.
data TypeConstr' i a = TypeConstr
  { _typeConstrInfo :: i,
    _typeConstrSymbol :: !Symbol,
    _typeConstrArgs :: ![a]
  }

-- | Dynamic type. A Node with a dynamic type has an unknown type. Useful
-- for transformations that introduce partial type information, e.g., one can
-- have types `* -> *` and `* -> * -> Nat` where `*` is the dynamic type.
newtype Dynamic' i = Dynamic {_dynamicInfo :: i}

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

instance HasAtomicity (Lambda' i a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Let' i a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (LetRec' i a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Case' i bi a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Pi' i a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Univ' i) where
  atomicity _ = Atom

instance HasAtomicity (TypeConstr' i a) where
  atomicity _ = Aggregate lambdaFixity

instance HasAtomicity (Dynamic' i) where
  atomicity _ = Atom

lambdaFixity :: Fixity
lambdaFixity = Fixity (PrecNat 0) (Unary AssocPostfix)

instance Eq (Var' i) where
  (Var _ idx1) == (Var _ idx2) = idx1 == idx2

instance Eq (Ident' i) where
  (Ident _ sym1) == (Ident _ sym2) = sym1 == sym2

instance Eq (Constant' i) where
  (Constant _ v1) == (Constant _ v2) = v1 == v2

instance Eq a => Eq (App' i a) where
  (App _ l1 r1) == (App _ l2 r2) = l1 == l2 && r1 == r2

instance (Eq f, Eq a) => Eq (Apps' f i a) where
  (Apps _ op1 args1) == (Apps _ op2 args2) = op1 == op2 && args1 == args2

instance Eq a => Eq (BuiltinApp' i a) where
  (BuiltinApp _ op1 args1) == (BuiltinApp _ op2 args2) = op1 == op2 && args1 == args2

instance Eq a => Eq (Constr' i a) where
  (Constr _ tag1 args1) == (Constr _ tag2 args2) = tag1 == tag2 && args1 == args2

instance Eq a => Eq (Lambda' i a) where
  (Lambda _ b1) == (Lambda _ b2) = b1 == b2

instance Eq a => Eq (Let' i a) where
  (Let _ v1 b1) == (Let _ v2 b2) = v1 == v2 && b1 == b2

instance Eq a => Eq (LetRec' i a) where
  (LetRec _ vs1 b1) == (LetRec _ vs2 b2) = vs1 == vs2 && b1 == b2

instance Eq a => Eq (Case' i bi a) where
  (Case _ v1 bs1 def1) == (Case _ v2 bs2 def2) = v1 == v2 && bs1 == bs2 && def1 == def2

instance Eq a => Eq (CaseBranch' i a) where
  (CaseBranch _ tag1 n1 b1) == (CaseBranch _ tag2 n2 b2) = tag1 == tag2 && n1 == n2 && b1 == b2

instance Eq a => Eq (Pi' i a) where
  (Pi _ ty1 b1) == (Pi _ ty2 b2) = ty1 == ty2 && b1 == b2

instance Eq (Univ' i) where
  (Univ _ l1) == (Univ _ l2) = l1 == l2

instance Eq a => Eq (TypeConstr' i a) where
  (TypeConstr _ sym1 args1) == (TypeConstr _ sym2 args2) = sym1 == sym2 && args1 == args2

instance Eq (Dynamic' i) where
  (Dynamic _) == (Dynamic _) = True

makeLenses ''Var'
makeLenses ''Ident'
makeLenses ''Constant'
makeLenses ''App'
makeLenses ''BuiltinApp'
makeLenses ''Constr'
makeLenses ''Let'
makeLenses ''LetRec'
makeLenses ''Case'
makeLenses ''Pi'
makeLenses ''Univ'
makeLenses ''TypeConstr'
makeLenses ''Dynamic'
makeLenses ''CaseBranch'

instance Hashable (Ident' i) where
  hashWithSalt s = hashWithSalt s . (^. identSymbol)

instance Hashable (Var' i) where
  hashWithSalt s = hashWithSalt s . (^. varIndex)
