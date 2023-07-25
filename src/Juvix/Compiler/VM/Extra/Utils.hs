module Juvix.Compiler.VM.Extra.Utils where

import Juvix.Compiler.VM.Language

mkBinop :: Opcode -> LValue -> Value -> Value -> Instruction
mkBinop op lval val1 val2 = Binop $ BinaryOp op lval val1 val2

mkMove :: LValue -> Value -> Instruction
mkMove lval val = Move $ InstrMove lval val

mkJump :: Value -> Instruction
mkJump val = Jump $ InstrJump val

mkJumpOnZero :: Value -> Value -> Instruction
mkJumpOnZero val dest = JumpOnZero $ InstrJumpOnZero val dest

mkLabel :: Text -> Instruction
mkLabel lab = Label $ InstrLabel lab

regSp :: Int
regSp = 0

regHp :: Int
regHp = 1

maxValueReg :: Value -> Int
maxValueReg = \case
  RegRef r -> r
  MemRef r -> r
  _ -> 0

maxLValueReg :: LValue -> Int
maxLValueReg = \case
  LRegRef r -> r
  LMemRef r -> r

maxInstrReg :: Instruction -> Int
maxInstrReg = \case
  Binop BinaryOp {..} ->
    maximum [maxLValueReg _binaryOpResult, maxValueReg _binaryOpArg1, maxValueReg _binaryOpArg2]
  Move InstrMove {..} ->
    max (maxLValueReg _instrMoveDest) (maxValueReg _instrMoveValue)
  Halt ->
    0
  Jump InstrJump {..} ->
    maxValueReg _instrJumpDest
  JumpOnZero InstrJumpOnZero {..} ->
    max (maxValueReg _instrJumpOnZeroValue) (maxValueReg _instrJumpOnZeroDest)
  Label {} ->
    0

computeRegsNum :: [Instruction] -> Int
computeRegsNum instrs = maximum (map maxInstrReg instrs) + 1
