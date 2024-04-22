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
  | ElementImmediate FField
  | ElementHint Hint

newtype Hint = Hint
  { _hintCode :: Text
  }

data Instruction = Instruction
  { _instrOffDst :: Offset,
    _instrOffOp0 :: Offset,
    _instrOffOp1 :: Offset,
    _instrDstReg :: Reg,
    _instrOp0Reg :: Reg,
    _instrOp1Src :: Op1Src,
    _instrResLogic :: ResLogic,
    _instrPcUpdate :: PcUpdate,
    _instrApUpdate :: ApUpdate,
    _instrOpcode :: Opcode
  }
  deriving stock (Show)

data Op1Src
  = Op1SrcOp0
  | Op1SrcImm
  | Op1SrcFp
  | Op1SrcAp
  deriving stock (Eq, Show)

data ResLogic
  = ResOp1
  | ResAdd
  | ResMul
  deriving stock (Eq, Show)

data PcUpdate
  = PcUpdateRegular
  | PcUpdateJump
  | PcUpdateJumpRel
  | PcUpdateJnz
  deriving stock (Eq, Show)

data ApUpdate
  = ApUpdateRegular
  | ApUpdateAdd
  | ApUpdateInc
  deriving stock (Eq, Show)

data Opcode
  = Nop
  | Call
  | Ret
  | AssertEq
  deriving stock (Eq, Show)

defaultInstruction :: Instruction
defaultInstruction =
  Instruction
    { _instrOffDst = -1,
      _instrOffOp0 = -1,
      _instrOffOp1 = -1,
      _instrDstReg = Ap,
      _instrOp0Reg = Ap,
      _instrOp1Src = Op1SrcAp,
      _instrResLogic = ResOp1,
      _instrPcUpdate = PcUpdateRegular,
      _instrApUpdate = ApUpdateRegular,
      _instrOpcode = Nop
    }

makeLenses ''Instruction
makeLenses ''Hint

elemSize :: Element -> Int
elemSize = \case
  ElementInstruction {} -> 1
  ElementImmediate {} -> 1
  ElementHint {} -> 0

elemsSize :: [Element] -> Int
elemsSize = sum . map elemSize
