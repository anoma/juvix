module Juvix.Compiler.Backend.Cairo.Language
  ( module Juvix.Compiler.Backend.Cairo.Language,
    module Juvix.Compiler.Casm.Language.Base,
    module Juvix.Data.Field,
  )
where

{-

This module defines data structures for the binary representation of Cairo
Assembly instructions, as defined in [1, Section 4]. See also:
https://github.com/lambdaclass/cairo-vm_in_go.

[1] Goldberg, Papini, Riabzev: "Cairo – a Turing-complete STARK-friendly CPU
    architecture" (https://ia.cr/2021/1063)

-}

import Juvix.Compiler.Casm.Language.Base
import Juvix.Data.Field

data Element
  = ElementInstruction Instruction
  | ElementConstant FField

data Instruction = Instruction
  { _instrOff0 :: Offset,
    _instrOff1 :: Offset,
    _instrOff2 :: Offset,
    _instrDstReg :: Reg,
    _instrOp0Reg :: Reg,
    _instrOp1Src :: Op1Src,
    _instrResLogic :: ResLogic,
    _instrPcUpdate :: PcUpdate,
    _instrOpcode :: Opcode
  }

data Op1Src
  = Op1SrcImm
  | Op1SrcAp
  | Op1SrcFp
  | Op1SrcOp0

data ResLogic
  = ResOp1
  | ResAdd
  | ResMul
  | ResUnconstrained

data PcUpdate
  = PcUpdateRegular
  | PcUpdateJump
  | PcUpdateJumpRel
  | PcUpdateJnz

data ApUpdate
  = ApUpdateRegular
  | ApUpdateAdd
  | ApUpdateAdd1
  | ApUpdateAdd2

data Opcode
  = Nop
  | AssertEq
  | Call
  | Ret

makeLenses ''Instruction
