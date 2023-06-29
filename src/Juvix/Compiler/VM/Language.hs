module Juvix.Compiler.VM.Language where

import Juvix.Compiler.VM.Language.Base

type SmallInt = Int

data Instruction = Instruction
  { _instructionOpcode :: Opcode,
    _instructionReg :: SmallInt,
    _instructionVal1 :: Value,
    _instructionVal2 :: Value
  }

data Value
  = Const SmallInt
  | RegRef SmallInt

-- Constructor representation: tag, field1, .., fieldn
--
-- Closure representation: addr, n, m, arg1, .., argn
--
-- Here `m` is the total number of arguments the function accepts and `n` is the
-- number of arguments actually stored in the closure.
--
-- The tag and the address can be read/written using ordinary load/store with
-- offset 0.

-- val: register reference or constant
-- dest, src, reg: register references
-- offset, tag, num, addr: constant
data Opcode
  = -- add dest, val1, val2
    OpIntAdd
  | -- sub dest, val1, val2
    OpIntSub
  | -- mul dest, val1, val2
    OpIntMul
  | -- div dest, val1, val2
    OpIntDiv
  | -- mod dest, val1, val2
    OpIntMod
  | -- lt dest, val1, val2
    OpIntLt
  | -- eq dest, val1, val2
    OpEq
  | -- load dest, src, offset
    -- Loads src[offset] (field at offset in data pointed to by src) into dest.
    OpLoad
  | -- store dest, offset, val
    OpStore
  | -- move dest, val, 0
    OpMove
  | -- halt 0, 0, 0
    OpHalt
  | -- alloc dest, num, 0
    -- Allocates `num` fields on the heap and stores the pointer in dest.
    OpAlloc
  | -- push src, 0, 0
    OpPush
  | -- pop dest, 0, 0
    OpPop
  | -- jump 0, val, 0
    OpJump
  | -- jumpz reg, val, 0
    -- Jumps to address `val` if the contents of register `reg` is 0.
    OpJumpOnZero
