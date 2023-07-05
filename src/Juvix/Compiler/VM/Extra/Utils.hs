module Juvix.Compiler.VM.Extra.Utils where

import Juvix.Compiler.VM.Language

mkBinop :: Opcode -> RegRef -> Value -> Value -> Instruction
mkBinop op reg val1 val2 = Binop $ BinaryOp op reg val1 val2

mkLoad :: RegRef -> RegRef -> SmallInt -> Instruction
mkLoad dest src off = Load $ InstrLoad dest src off

mkStore :: RegRef -> SmallInt -> Value -> Instruction
mkStore dest off val = Store $ InstrStore dest off val

mkMove :: RegRef -> Value -> Instruction
mkMove reg val = Move $ InstrMove reg val

mkAlloc :: RegRef -> Value -> Instruction
mkAlloc reg val = Alloc $ InstrAlloc reg val

mkPush :: Value -> Instruction
mkPush val = Push $ InstrPush val

mkPop :: RegRef -> Instruction
mkPop reg = Pop $ InstrPop reg

mkJump :: Value -> Instruction
mkJump val = Jump $ InstrJump val

mkJumpOnZero :: RegRef -> Value -> Instruction
mkJumpOnZero reg val = JumpOnZero $ InstrJumpOnZero reg val

mkLabel :: Text -> Instruction
mkLabel lab = Label $ InstrLabel lab

maxValueReg :: Value -> Int
maxValueReg = \case
  RegRef r -> r
  _ -> 0

maxInstrReg :: Instruction -> Int
maxInstrReg = \case
  Binop BinaryOp {..} ->
    maximum [_binaryOpResult, maxValueReg _binaryOpArg1, maxValueReg _binaryOpArg2]
  Load InstrLoad {..} ->
    max _instrLoadDest _instrLoadSrc
  Store InstrStore {..} ->
    max _instrStoreDest (maxValueReg _instrStoreValue)
  Move InstrMove {..} ->
    max _instrMoveDest (maxValueReg _instrMoveValue)
  Halt ->
    0
  Alloc InstrAlloc {..} ->
    _instrAllocDest
  Push InstrPush {..} ->
    maxValueReg _instrPushValue
  Pop InstrPop {..} ->
    _instrPopDest
  Jump InstrJump {..} ->
    maxValueReg _instrJumpDest
  JumpOnZero InstrJumpOnZero {..} ->
    max _instrJumpOnZeroReg (maxValueReg _instrJumpOnZeroDest)
  Label {} ->
    0

computeRegsNum :: [Instruction] -> Int
computeRegsNum instrs = maximum (map maxInstrReg instrs) + 1
