module Juvix.Compiler.Reg.Language.Instrs
  ( module Juvix.Compiler.Reg.Language.Instrs,
    module Juvix.Compiler.Reg.Language.Base,
  )
where

import Juvix.Compiler.Reg.Language.Base

data Value
  = ValConst Constant
  | CRef ConstrField
  | VRef VarRef

-- | Reference to a constructor field (argument).
data ConstrField = ConstrField
  { -- | Tag of the constructor being referenced.
    _constrFieldTag :: Tag,
    -- | Memory representation of the constructor.
    _constrFieldMemRep :: MemRep,
    -- | Location where the data is stored.
    _constrFieldRef :: VarRef,
    -- | Index of the constructor argument being referenced.
    _constrFieldIndex :: Index
  }

data VarGroup
  = VarGroupArgs
  | VarGroupLocal
  deriving stock (Eq, Ord, Generic, Show)

instance Hashable VarGroup

data VarRef = VarRef
  { _varRefGroup :: VarGroup,
    _varRefIndex :: Index,
    _varRefName :: Maybe Text
  }
  deriving stock (Show)

makeLenses ''VarRef
makeLenses ''ConstrField

instance Hashable VarRef where
  hashWithSalt salt VarRef {..} = hashWithSalt salt (_varRefGroup, _varRefIndex)

instance Eq VarRef where
  vr1 == vr2 =
    vr1 ^. varRefGroup == vr2 ^. varRefGroup
      && vr1 ^. varRefIndex == vr2 ^. varRefIndex

instance Ord VarRef where
  compare vr1 vr2 =
    compare (vr1 ^. varRefGroup, vr1 ^. varRefIndex) (vr2 ^. varRefGroup, vr2 ^. varRefIndex)

deriving stock instance (Eq ConstrField)

deriving stock instance (Eq Value)

data InstrBinop = InstrBinop
  { _instrBinopOpcode :: BinaryOp,
    _instrBinopResult :: VarRef,
    _instrBinopArg1 :: Value,
    _instrBinopArg2 :: Value
  }
  deriving stock (Eq)

data InstrUnop = InstrUnop
  { _instrUnopOpcode :: UnaryOp,
    _instrUnopResult :: VarRef,
    _instrUnopArg :: Value
  }
  deriving stock (Eq)

data InstrCairo = InstrCairo
  { _instrCairoOpcode :: CairoOp,
    _instrCairoResult :: VarRef,
    _instrCairoArgs :: [Value]
  }
  deriving stock (Eq)

data InstrAssign = InstrAssign
  { _instrAssignResult :: VarRef,
    _instrAssignValue :: Value
  }
  deriving stock (Eq)

newtype InstrAssert = InstrAssert
  { _instrAssertValue :: Value
  }
  deriving stock (Eq)

newtype InstrTrace = InstrTrace
  { _instrTraceValue :: Value
  }
  deriving stock (Eq)

newtype InstrFailure = InstrFailure
  { _instrFailureValue :: Value
  }
  deriving stock (Eq)

data InstrAlloc = InstrAlloc
  { _instrAllocResult :: VarRef,
    _instrAllocTag :: Tag,
    _instrAllocMemRep :: MemRep,
    _instrAllocArgs :: [Value]
  }
  deriving stock (Eq)

data InstrAllocClosure = InstrAllocClosure
  { _instrAllocClosureResult :: VarRef,
    _instrAllocClosureSymbol :: Symbol,
    _instrAllocClosureExpectedArgsNum :: Int,
    _instrAllocClosureArgs :: [Value]
  }
  deriving stock (Eq)

data InstrExtendClosure = InstrExtendClosure
  { _instrExtendClosureResult :: VarRef,
    _instrExtendClosureValue :: VarRef,
    _instrExtendClosureArgs :: [Value]
  }
  deriving stock (Eq)

data CallType
  = CallFun Symbol
  | CallClosure VarRef
  deriving stock (Eq)

data InstrCall = InstrCall
  { _instrCallResult :: VarRef,
    _instrCallType :: CallType,
    _instrCallArgs :: [Value],
    -- | Variables live after the call. Live variables need to be
    -- saved before the call and restored after it.
    _instrCallLiveVars :: [VarRef]
  }
  deriving stock (Eq)

data InstrTailCall = InstrTailCall
  { _instrTailCallType :: CallType,
    _instrTailCallArgs :: [Value]
  }
  deriving stock (Eq)

newtype InstrReturn = InstrReturn
  { _instrReturnValue :: Value
  }
  deriving stock (Eq)

data InstrIf' a = InstrIf
  { _instrIfOp :: BoolOp,
    _instrIfArg1 :: Value,
    _instrIfArg2 :: Value,
    _instrIfTrue :: a,
    _instrIfFalse :: a,
    -- | Output variable storing the result (corresponds to the top of the value
    -- stack in JuvixAsm after executing the branches)
    _instrIfOutVar :: Maybe VarRef
  }
  deriving stock (Eq, Functor)

data InstrBranch' a = InstrBranch
  { _instrBranchValue :: Value,
    _instrBranchTrue :: a,
    _instrBranchFalse :: a,
    -- | Output variable storing the result (corresponds to the top of the value
    -- stack in JuvixAsm after executing the branches)
    _instrBranchOutVar :: Maybe VarRef
  }
  deriving stock (Eq, Functor)

data InstrCase' a = InstrCase
  { _instrCaseValue :: Value,
    _instrCaseInductive :: Symbol,
    _instrCaseIndRep :: IndRep,
    _instrCaseBranches :: [CaseBranch' a],
    _instrCaseDefault :: Maybe a,
    _instrCaseOutVar :: Maybe VarRef
  }
  deriving stock (Eq, Functor)

data CaseBranch' a = CaseBranch
  { _caseBranchTag :: Tag,
    -- | Memory representation of the constructor corresponding to the branch.
    _caseBranchMemRep :: MemRep,
    _caseBranchArgsNum :: Int,
    _caseBranchCode :: a
  }
  deriving stock (Eq, Functor)

makeLenses ''InstrBinop
makeLenses ''InstrUnop
makeLenses ''InstrCairo
makeLenses ''InstrAssign
makeLenses ''InstrAssert
makeLenses ''InstrTrace
makeLenses ''InstrFailure
makeLenses ''InstrAlloc
makeLenses ''InstrAllocClosure
makeLenses ''InstrExtendClosure
makeLenses ''InstrReturn
makeLenses ''InstrTailCall
makeLenses ''InstrCall
makeLenses ''InstrIf'
makeLenses ''InstrBranch'
makeLenses ''InstrCase'
makeLenses ''CaseBranch'

mkVarRef :: VarGroup -> Index -> VarRef
mkVarRef g i =
  VarRef
    { _varRefGroup = g,
      _varRefIndex = i,
      _varRefName = Nothing
    }
