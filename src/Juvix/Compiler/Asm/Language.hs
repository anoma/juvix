-- |
-- A JuvixAsm program is a set of JuvixAsm functions. Every function has an
-- associated Symbol, a fixed (i.e., known at compile time) number of arguments, a
-- fixed temporary stack size, and a fixed maximum local value stack height (see
-- Interpreter/RuntimeState.hs).
--
-- See the file Interpreter/RuntimeState.hs for a description of JuvixAsm runtime
-- and memory layout.
module Juvix.Compiler.Asm.Language
  ( module Juvix.Compiler.Asm.Language,
    module Juvix.Compiler.Core.Language.Base,
  )
where

import Juvix.Compiler.Core.Language.Base

-- In what follows, when referring to the stack we mean the current local value
-- stack, unless otherwise stated. By stack[n] we denote the n-th cell from the
-- top in the value stack (0-based).

-- | Offset of a data field or an argument
type Offset = Int

-- | Values reference readable values (constant or value stored in memory). Void
-- is an unprintable unit.
data Value
  = ConstInt Integer
  | ConstBool Bool
  | ConstString Text
  | ConstUnit
  | ConstVoid
  | Ref MemValue

-- | MemValues are references to values stored in random-access memory.
data MemValue
  = -- | A direct memory reference.
    DRef DirectRef
  | -- | ConstrRef is an indirect reference to a field (argument) of
    --  a constructor: field k holds the (k+1)th argument.
    ConstrRef Field

-- | DirectRef is a direct memory reference.
data DirectRef
  = -- | StackRef references the top of the stack. JVA code: '$'.
    StackRef
  | -- | ArgRef references an argument in the argument area (0-based offsets).
    --   JVA code: 'arg[<offset>]'.
    ArgRef Offset
  | -- | TempRef references a value in the temporary area (0-based offsets). JVA
    --   code: 'tmp[<offset>]'.
    TempRef Offset

-- | Constructor field reference. JVA code: '<dref>.<tag>[<offset>]'
data Field = Field
  { -- | tag of the constructor being referenced
    _fieldTag :: Tag,
    -- | location where the data is stored
    _fieldRef :: DirectRef,
    _fieldOffset :: Offset
  }

makeLenses ''Field

-- | Function call type
data CallType = CallFun Symbol | CallClosure
  deriving stock (Eq)

-- | `Instruction` is a single non-branching instruction, i.e., with no control
-- transfer.
data Instruction
  = -- | An instruction which takes its operands from the two top stack cells,
    -- pops the stack by two, and then pushes the result.
    Binop Opcode
  | -- | Push a value on top of the stack. JVA opcode: 'push <val>'.
    Push Value
  | -- | Pop the stack. JVA opcode: 'pop'.
    Pop
  | -- | Push the top of the value stack onto the temporary stack, pop the value
    -- stack. Used to implement Core.Let and Core.Case. JVA opcodes: 'pusht', 'popt'.
    PushTemp
  | PopTemp
  | -- | Print a debug log of the object on top of the stack. Does not pop the
    -- stack. JVA opcode: 'trace'.
    Trace
  | -- | Dump the stacktrace. JVA opcode: 'dump'.
    Dump
  | -- | Interrupt execution with a runtime error printing the value on top of
    -- the stack. JVA opcode: 'fail'.
    Failure
  | -- | Preallocate memory. This instruction is inserted automatically before
    -- translation to JuvixReg. It does not occur in JVA files.
    Prealloc InstrPrealloc
  | -- | Allocate constructor data with a given tag. The n arguments (the number n
    -- determined by the constant tag) are popped from the stack and stored at
    -- increasing offsets (stack[0]: field 0, stack[1]: field 1, ...,
    -- stack[n-1]: field n-1). The data is pushed on top of the stack. JVA
    -- opcode: 'alloc <tag>'.
    AllocConstr Tag
  | -- | Allocate a closure for the given function symbol. n = allocClosureArgsNum
    -- indicates the number of function arguments available (strictly less than
    -- the number of arguments expected by the function -- known functions can
    -- take zero arguments, but closures are required to take at least one). The
    -- n function arguments are popped from the stack and stored in the closure
    -- at increasing offsets. The result is pushed on top of the stack. JVA
    -- opcode: 'calloc <fun> <int>'.
    AllocClosure InstrAllocClosure
  | -- | Extend a closure on top of the stack with more arguments. n =
    -- extendClosureArgsNum indicates the number of arguments to extend the
    -- closure with -- it must be less than the number of arguments expected by
    -- the closure. Pops the closure from the stack, pops n additional arguments
    -- from the stack and extends the closure with them in increasing order,
    -- then pushes the extended closure on top of the stack. JVA opcode:
    -- 'cextend <int>'.
    ExtendClosure InstrExtendClosure
  | -- | Call a function given by an immediate constant Symbol or a closure on top
    -- of the stack. Creates a new activation frame for the function. The n =
    -- callArgsNum function arguments are popped from the stack and stored at
    -- increasing offsets (offset 0: top of stack) in the argument area. The
    -- return address is pushed on the global call stack. If the callType is
    -- CallClosure, then the closure is fetched from the top of the stack, the
    -- arguments stored in the closure are transferred to the argument area in
    -- increasing offset order, and then the supplied callArgsNum arguments are
    -- popped from the stack and transferred to the argument area in increasing
    -- order. JVA opcode: 'call <fun>' or 'call $ <int>'
    Call InstrCall
  | -- | Same as `Call`, but does not push the call stack, discarding the current
    -- activation frame instead. JVA opcode: 'tcall <fun>' or 'tcall $ <int>'
    TailCall InstrCall
  | -- | 'CallClosures' and 'TailCallClosures' are like 'Call' and 'TailCall'
    -- with 'CallClosure' call type, except that (1) they either call or extend
    -- the closure depending on the number of supplied arguments
    -- (callClosureArgsNum) vs the number of expected arguments fetched at
    -- runtime from the closure, and (2) if the number of expected arguments is
    -- smaller than the number of supplied arguments, then the result of the
    -- call must be another closure and the process is repeated until we run out
    -- of supplied arguments. With 'TailCallClosures', if the last operation is
    -- a call then it is a tail call, and if the last operation is a closure
    -- extension, then an implicit 'Return' is executed after it. JVA opcodes:
    -- 'ccall <int>', 'tccall <int>'.
    CallClosures InstrCallClosures
  | TailCallClosures InstrCallClosures
  | -- | Pushes the top of the current value stack on top of the calling function
    -- value stack, discards the current activation frame, transfers control to
    -- the address at the top of the global call stack, and pops the call stack.
    -- JVA opcode: 'ret'.
    Return

data Opcode
  = -- | Add stack[0] + stack[1], pop the stack by two, and push the result. JVA
    -- opcode: 'add'.
    IntAdd
  | -- | Subtract stack[0] - stack[1], pop the stack by two, and push the
    -- result. JVA opcode: 'sub'.
    IntSub
  | -- | Multiply stack[0] * stack[1], pop the stack by two, and push the
    -- result. JVA opcode 'mul'.
    IntMul
  | -- | Divide stack[0] / stack[1], pop the stack by two, and push the result.
    -- JVA opcode: 'div'.
    IntDiv
  | -- | Calculate modulus stack[0] % stack[1], pop the stack by two, and push
    -- the result. JVA opcode: 'mod'.
    IntMod
  | -- | Compare stack[0] < stack[1], pop the stack by two, and push the result.
    -- JVA opcode: 'lt'.
    IntLt
  | -- | Compare stack[0] <= stack[1], pop the stack by two, and push the
    -- result. JVA opcode: 'le'.
    IntLe
  | -- | Compare the two top stack cells with structural equality, pop the stack
    -- by two, and push the result. JVA opcode: 'eq'.
    ValEq

newtype InstrPrealloc = InstrPrealloc
  { -- | How many words to preallocate?
    _preallocWordsNum :: Int
  }

data InstrAllocClosure = InstrAllocClosure
  { _allocClosureFunSymbol :: Symbol,
    -- | The number of supplied arguments to be stored in the closure.
    _allocClosureArgsNum :: Int
  }

newtype InstrExtendClosure = InstrExtendClosure
  { -- | The number of supplied arguments by which the closure is to be extended.
    _extendClosureArgsNum :: Int
  }

data InstrCall = InstrCall
  { _callType :: CallType,
    -- | The number of arguments supplied to the call.
    _callArgsNum :: Int
  }

newtype InstrCallClosures = InstrCallClosures
  { -- | The number of arguments supplied to the call. This does not include the
    -- called closure on top of the stack.
    _callClosuresArgsNum :: Int
  }

-- | `Command` consists of a single non-branching instruction or a single branching
-- command together with the branches.
data Command
  = -- | A single non-branching instruction.
    Instr CmdInstr
  | -- | Branch based on a boolean value on top of the stack, pop the stack. JVA
    -- code: 'br { true: {<code>} false: {<code>} }'.
    Branch CmdBranch
  | -- | Branch based on the tag of the constructor data on top of the stack. Does
    -- _not_ pop the stack. The second argument is the optional default branch.
    -- JVA code: 'case <ind> { <tag>: {<code>} ... <tag>: {<code>} default: {<code>} }'
    -- (any branch may be omitted).
    Case CmdCase

newtype CommandInfo = CommandInfo
  { _commandInfoLocation :: Maybe Location
  }

emptyInfo :: CommandInfo
emptyInfo = CommandInfo Nothing

data CmdInstr = CmdInstr
  { _cmdInstrInfo :: CommandInfo,
    _cmdInstrInstruction :: Instruction
  }

data CmdBranch = CmdBranch
  { _cmdBranchInfo :: CommandInfo,
    _cmdBranchTrue :: Code,
    _cmdBranchFalse :: Code
  }

data CmdCase = CmdCase
  { _cmdCaseInfo :: CommandInfo,
    _cmdCaseInductive :: Symbol,
    _cmdCaseBranches :: [CaseBranch],
    _cmdCaseDefault :: Maybe Code
  }

data CaseBranch = CaseBranch
  { _caseBranchTag :: Tag,
    _caseBranchCode :: Code
  }

-- | `Code` corresponds to JuvixAsm code for a single function.
type Code = [Command]

makeLenses ''InstrAllocClosure
makeLenses ''InstrExtendClosure
makeLenses ''InstrCall
makeLenses ''InstrCallClosures
makeLenses ''CommandInfo
makeLenses ''CmdInstr
makeLenses ''CmdBranch
makeLenses ''CmdCase
makeLenses ''CaseBranch
