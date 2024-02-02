module Juvix.Compiler.Tree.Language
  ( module Juvix.Compiler.Tree.Language,
    module Juvix.Compiler.Tree.Language.Base,
  )
where

import Juvix.Compiler.Tree.Language.Base

-- | Function call type
data CallType = CallFun Symbol | CallClosure Node

data Node
  = Binop NodeBinop
  | Unop NodeUnop
  | -- | A constant value.
    Const NodeConstant
  | -- | A memory reference.
    MemRef NodeMemRef
  | -- | Allocate constructor data. JVT code: 'alloc[<tag>](x1, .., xn)'.
    AllocConstr NodeAllocConstr
  | -- | Allocate a closure. JVT code: 'calloc[<fun>](x1, .., xn)'.
    AllocClosure NodeAllocClosure
  | -- | Extend a closure with more arguments. JVT code: 'cextend(cl, x1, .., xn)'.
    ExtendClosure NodeExtendClosure
  | -- | Call a function given by an immediate constant Symbol or a closure. JVT
    -- code: 'call[<fun>](x1, .., xn)' or 'call(cl, x1, .., xn)'
    Call NodeCall
  | -- | 'CallClosures' is like 'Call' with 'CallClosure' call type, except that
    -- (1) it either calls or extends the closure depending on the number of
    -- supplied arguments vs the number of expected arguments fetched at runtime
    -- from the closure, and (2) if the number of expected arguments is smaller
    -- than the number of supplied arguments, then the result of the call must
    -- be another closure and the process is repeated until we run out of
    -- supplied arguments. JVT code: 'ccall(cl, x1, .., xn)'.
    CallClosures NodeCallClosures
  | -- | Branch based on a boolean value. JVT code: 'br(x) { true: <code>;
    -- false: <code> }'.
    Branch NodeBranch
  | -- | Branch based on the tag of constructor data.
    -- JVT code: 'case[<ind>](x) { <tag>: <code>; ... <tag>: <code>; default: <code> }'
    -- (any branch may be omitted).
    Case NodeCase
  | -- | Execute nested code with temporary stack extended with a given value.
    -- Used to implement Core.Let. JVT codes: 'save(x) {<code>}',
    -- 'save[<name>](x) {<code>}'.
    Save NodeSave

newtype NodeInfo = NodeInfo
  { _nodeInfoLocation :: Maybe Location
  }
  deriving newtype (Semigroup, Monoid)

data BinaryOpcode
  = IntAdd
  | IntSub
  | IntMul
  | IntDiv
  | IntMod
  | IntLt
  | IntLe
  | ValEq
  | StrConcat
  | -- | Sequence: evaluate and ignore fist argument, return evaluated second
    -- argument. JVT code: 'seq(x1, x2)'.
    OpSeq

data UnaryOpcode
  = -- | Convert the argument to a string. JVT code: 'show(x)'.
    OpShow
  | -- | Convert a string to an integer. JVT opcode: 'atoi(x)'.
    OpStrToInt
  | -- | Print a debug log of the argument and return it. JVT code: 'trace(x)'.
    OpTrace
  | -- | Interrupt execution with a runtime error printing the argument. JVT
    -- code: 'fail(x)'.
    OpFail
  | -- | Compute the number of expected arguments for the given closure. JVT
    -- code: 'argsnum(x)'.
    OpArgsNum

data NodeBinop = NodeBinop
  { _nodeBinopInfo :: NodeInfo,
    _nodeBinopOpcode :: BinaryOpcode,
    _nodeBinopArg1 :: Node,
    _nodeBinopArg2 :: Node
  }

data NodeUnop = NodeUnop
  { _nodeUnopInfo :: NodeInfo,
    _nodeUnopOpcode :: UnaryOpcode,
    _nodeUnopArg :: Node
  }

data NodeConstant = NodeConstant
  { _nodeConstantInfo :: NodeInfo,
    _nodeConstant :: Constant
  }

data NodeMemRef = NodeMemRef
  { _nodeMemRefInfo :: NodeInfo,
    _nodeMemRef :: MemRef
  }

data NodeAllocConstr = NodeAllocConstr
  { _nodeAllocConstrInfo :: NodeInfo,
    _nodeAllocConstrTag :: Tag,
    _nodeAllocConstrArgs :: [Node]
  }

data NodeAllocClosure = NodeAllocClosure
  { _nodeAllocClosureInfo :: NodeInfo,
    _nodeAllocClosureFunSymbol :: Symbol,
    _nodeAllocClosureArgs :: [Node]
  }

data NodeExtendClosure = NodeExtendClosure
  { _nodeExtendClosureInfo :: NodeInfo,
    _nodeExtendClosureFun :: Node,
    _nodeExtendClosureArgs :: NonEmpty Node
  }

data NodeCall = NodeCall
  { _nodeCallInfo :: NodeInfo,
    _nodeCallType :: CallType,
    _nodeCallArgs :: [Node]
  }

data NodeCallClosures = NodeCallClosures
  { _nodeCallClosuresInfo :: NodeInfo,
    _nodeCallClosuresFun :: Node,
    _nodeCallClosuresArgs :: NonEmpty Node
  }

data NodeBranch = NodeBranch
  { _nodeBranchInfo :: NodeInfo,
    _nodeBranchArg :: Node,
    _nodeBranchTrue :: Node,
    _nodeBranchFalse :: Node
  }

data NodeCase = NodeCase
  { _nodeCaseInfo :: NodeInfo,
    _nodeCaseInductive :: Symbol,
    _nodeCaseArg :: Node,
    _nodeCaseBranches :: [CaseBranch],
    _nodeCaseDefault :: Maybe Node
  }

data CaseBranch = CaseBranch
  { _caseBranchLocation :: Maybe Location,
    _caseBranchTag :: Tag,
    _caseBranchBody :: Node,
    -- | Indicates whether the evaluated case argument should be pushed onto the
    -- temporary stack in this branch.
    _caseBranchSave :: Bool
  }

data TempVar = TempVar
  { _tempVarName :: Maybe Text,
    _tempVarLocation :: Maybe Location
  }

data NodeSave = NodeSave
  { _nodeSaveInfo :: NodeInfo,
    _nodeSaveTempVar :: TempVar,
    _nodeSaveArg :: Node,
    _nodeSaveBody :: Node
  }

makeLenses ''NodeBinop
makeLenses ''NodeUnop
makeLenses ''NodeConstant
makeLenses ''NodeMemRef
makeLenses ''NodeAllocClosure
makeLenses ''NodeExtendClosure
makeLenses ''NodeCall
makeLenses ''NodeCallClosures
makeLenses ''NodeBranch
makeLenses ''NodeCase
makeLenses ''NodeSave
makeLenses ''TempVar
makeLenses ''CaseBranch
makeLenses ''NodeInfo
