module Juvix.Compiler.VM.Serialization.ToVampIR (serialize, LabelError (..)) where

import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Juvix.Compiler.VM.Extra.Labels
import Juvix.Compiler.VM.Extra.Utils (computeRegsNum)
import Juvix.Compiler.VM.Language
import Juvix.Compiler.VM.Options

serialize :: Member (Error LabelError) r => Options -> [Instruction] -> Sem r ByteString
serialize opts instrs0 = do
  instrs <- resolveLabels instrs0
  let code = BS.concat (fmap go instrs) <> "[]"
  return $
    vampirPrelude regsNum opts
      <> "run stepsNum ("
      <> code
      <> ") = 1;\n"
  where
    regsNum :: Int
    regsNum = computeRegsNum instrs0

    go :: Instruction -> ByteString
    go = \case
      Binop x -> goBinop x
      JumpOnZero x -> goJumpOnZero x
      Halt -> goHalt
      Jump x -> goJump x
      Move x -> goMove x
      Label {} -> impossible

    quad :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString
    quad op arg0 arg1 arg2 =
      "("
        <> op
        <> ", "
        <> arg0
        <> ", "
        <> arg1
        <> ", "
        <> arg2
        <> "):"

    goValue :: Value -> ByteString
    goValue = \case
      Const x -> "Cst " <> show x
      RegRef x -> "Reg " <> show x
      MemRef x -> "Mem " <> show x
      VarRef x -> "Cst (" <> fromText x <> " + 0)"
      LabelRef {} -> impossible

    goLValue :: LValue -> ByteString
    goLValue = \case
      LRegRef x -> "LReg " <> show x
      LMemRef x -> "LMem " <> show x

    goOpcode :: Opcode -> ByteString
    goOpcode = \case
      OpIntAdd -> "OpIntAdd"
      OpIntSub -> "OpIntSub"
      OpIntMul -> "OpIntMul"
      OpIntDiv -> error "OpIntDiv not implemented"
      OpIntMod -> error "OpIntMod not implemented"
      OpIntLt -> "OpIntLt"
      OpIntEq -> "OpIntEq"

    goBinop :: BinaryOp -> ByteString
    goBinop BinaryOp {..} =
      quad
        (goOpcode _binaryOpCode)
        (goLValue _binaryOpResult)
        (goValue _binaryOpArg1)
        (goValue _binaryOpArg2)

    goMove :: InstrMove -> ByteString
    goMove InstrMove {..} =
      quad
        "OpIntAdd"
        (goLValue _instrMoveDest)
        (goValue _instrMoveValue)
        "Cst 0"

    goHalt :: ByteString
    goHalt = quad "OpHalt" "0" "0" "0"

    goJump :: InstrJump -> ByteString
    goJump InstrJump {..} =
      quad
        "OpJumpOnZero"
        ("LReg " <> show regsNum)
        "Cst 0"
        (goValue _instrJumpDest)

    goJumpOnZero :: InstrJumpOnZero -> ByteString
    goJumpOnZero InstrJumpOnZero {..} =
      quad
        "OpJumpOnZero"
        ("LReg " <> show regsNum)
        (goValue _instrJumpOnZeroValue)
        (goValue _instrJumpOnZeroDest)

vampirPrelude :: Int -> Options -> ByteString
vampirPrelude regsNum opts =
  "def integerBits = "
    <> show (opts ^. optIntegerBits)
    <> ";\n"
    <> "def regsNum = "
    <> show regsNum
    <> ";\n"
    <> "def stackSize = "
    <> show (max (opts ^. optStackSize) 1)
    <> ";\n"
    <> "def heapSize = "
    <> show (opts ^. optHeapSize)
    <> ";\n"
    <> "def stepsNum = "
    <> show (opts ^. optStepsNum)
    <> ";\n\n"
    <> $(FE.makeRelativeToProject "runtime/src/vampir/stdlib.pir" >>= FE.embedFile)
    <> "\n"
    <> $(FE.makeRelativeToProject "runtime/src/vampir/vm.pir" >>= FE.embedFile)
    <> "\n"
