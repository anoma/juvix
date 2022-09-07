{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Compiler.Asm.Language
  ( module Juvix.Compiler.Asm.Language,
    module Juvix.Compiler.Asm.Language.Type,
    module Juvix.Compiler.Core.Language.Base,
    Stack,
  )
where

import Juvix.Compiler.Asm.Data.Stack (Stack)
import Juvix.Compiler.Asm.Language.Type
import Juvix.Compiler.Core.Language.Base

{-
A JuvixAsm program is a set of JuvixAsm functions. Every function has an
associated Symbol, a fixed (i.e., known at compile time) number of arguments, a
fixed temporary area size, and a fixed maximum local value stack height (see
Interpreter/Runtime.hs).

See the file Interpreter/Runtime.hs for a description of JuvixAsm runtime and
memory layout.
-}

-- In what follows, when referring to the stack we mean the current local value
-- stack, unless otherwise stated. By stack[n] we denote the n-th cell from the
-- top in the value stack.

-- | Offset of a data field or an argument
type Offset = Int

-- | Values reference readable values (constant or value stored in memory).
data Value = ConstInt Integer | ConstBool Bool | ConstString Text | Ref MemValue

{-
- MemValues are references to values stored in random-access memory.
- StackRef references the top of the stack.
- ArgRef references an argument in the argument area: offset n references the
  (n+1)th argument.
- TempRef references a value in the temporary area.
- ConstrRef references a field (argument) of a constructor: field k holds the
  (k+1)th argument.
-}
data MemValue = StackRef | ArgRef Offset | TempRef Offset | ConstrRef Field

data Field = Field
  { _fieldValue :: MemValue, -- location where the data is stored
    _fieldOffset :: Offset -- field offset
  }

makeLenses ''Field

-- | Function call type
data CallType = CallFun Symbol | CallClosure

-- | `Instruction` is a single non-branching instruction, i.e., with no control
-- transfer.
data Instruction
  = -- | Add stack[1] - stack[0], pop the stack by two, and push the result.
    IntAdd
  | -- | Subtract stack[1] - stack[0], pop the stack by two, and push the result.
    IntSub
  | -- | Multiply stack[1] * stack[0], pop the stack by two, and push the result.
    IntMul
  | -- | Divide stack[1] / stack[0], pop the stack by two, and push the result.
    IntDiv
  | -- | Compare stack[1] < stack[0], pop the stack by two, and push the result.
    IntLt
  | -- | Compare stack[1] <= stack[0], pop the stack by two, and push the result.
    IntLe
  | -- | Compare the two top stack cells with structural equality, pop the stack
    -- by two, and push the result.
    ValEq
  | -- | Push a value on top of the stack.
    Push Value
  | -- | Pop the stack.
    Pop
  | -- | Push the top of the value stack onto the temporary stack, pop the value
    -- stack. Used to implement Core.Let and Core.Case.
    PushTemp
  | PopTemp
  | -- | Allocate constructor data with a given tag. The n arguments (the number n
    -- determined by the constant tag) are popped from the stack and stored at
    -- _decreasing_ offsets (stack[0]: field n-1, stack[1]: field n-2, ...,
    -- stack[n-1]: field 0). The data is pushed on top of the stack.
    AllocConstr Tag
  | -- | Allocate a closure for the given function symbol. n = allocClosureArgsNum
    -- indicates the number of function arguments available (strictly less than
    -- the number of arguments expected by the function). The n function
    -- arguments are popped from the stack and stored in the closure at
    -- _decreasing_ offsets. The result is pushed on top of the stack.
    AllocClosure InstrAllocClosure
  | -- | Extend a closure on top of the stack with more arguments. n =
    -- extendClosureArgsNum indicates the number of arguments to extend the
    -- closure with -- it must be less than the number of arguments expected by
    -- the closure. Pops the closure from the stack, pops n additional arguments
    -- from the stack and extends the closure with them in _decreasing_ order,
    -- then pushes the extended closure on top of the stack.
    ExtendClosure InstrExtendClosure
  | -- | Call a function given by an immediate constant Symbol or a closure on top
    -- of the stack. Creates a new activation frame for the function. The n =
    -- callArgsNum function arguments are popped from the stack and stored at
    -- _decreasing_ offsets (offset n-1: top of stack) in the argument area. The
    -- return address is pushed on the global call stack. If the callType is
    -- CallClosure, then the closure is fetched from the top of the stack, the
    -- arguments stored in the closure are transferred to the argument area in
    -- _increasing_ offset order, and then the supplied callArgsNum arguments
    -- are popped from the stack and transferred to the argument area in
    -- _decreasing_ order.
    Call InstrCall
  | -- | Same as `Call`, but does not push the call stack, discarding the current
    -- activation frame instead.
    TailCall InstrCall
  | -- | `CallClosures` and `TailCallClosures` are like `Call` and `TailCall`
    -- with `CallClosure` call type, except that (1) they either call or extend
    -- the closure depending on the number of supplied arguments
    -- (callClosureArgsNum) vs the number of expected arguments fetched at
    -- runtime from the closure, and (2) if the number of expected arguments is
    -- smaller than the number of supplied arguments, then the result of the
    -- call must be another closure and the process is repeated until we run out
    -- of supplied arguments. With `TailCallClosures`, if the last operation is
    -- a call then it is a tail call, and if the last operation is a closure
    -- extensions, then an implicit Return is executed after it.
    CallClosures InstrCallClosures
  | TailCallClosures InstrCallClosures
  | -- | Pushes the top of the current value stack on top of the calling function
    -- value stack, discards the current activation frame, transfers control to
    -- the address at the top of the global call stack, and pops the call stack.
    Return

data InstrAllocClosure = InstrAllocClosure
  { _allocClosureFunSymbol :: Symbol,
    _allocClosureArgsNum :: Int
  }

newtype InstrExtendClosure = InstrExtendClosure
  { _extendClosureArgsNum :: Int
  }

data InstrCall = InstrCall
  { _callType :: CallType,
    _callArgsNum :: Int
  }

newtype InstrCallClosures = InstrCallClosures
  { _callClosuresArgsNum :: Int
  }

-- | `Command` consists of a single non-branching instruction or a single branching
-- command together with the branches.
data Command
  = -- | A single non-branching instruction.
    Instr CmdInstr
  | -- | Branch based on a boolean value on top of the stack, pop the stack.
    Branch CmdBranch
  | -- | Branch based on the tag of the constructor data on top of the stack. Does
    -- _not_ pop the stack. The second argument is the optional default branch.
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
