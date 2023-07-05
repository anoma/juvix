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
  let code = BS.concat (fmap go instrs) <> "nil"
  return $
    vampirPrelude (computeRegsNum instrs) opts
      <> "run ("
      <> show (opts ^. optStepsNum)
      <> ")"
      <> " ("
      <> code
      <> ") = 1;"
  where
    go :: Instruction -> ByteString
    go = \case
      Binop x -> goBinop x
      Load x -> goLoad x
      Store x -> goStore x
      Move x -> goMove x
      Halt -> goHalt
      Alloc x -> goAlloc x
      Push x -> goPush x
      Pop x -> goPop x
      Jump x -> goJump x
      JumpOnZero x -> goJumpOnZero x
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

    goBinop :: BinaryOp -> ByteString
    goBinop BinaryOp {..} =
      quad
        (goOpcode _binaryOpCode)
        (show _binaryOpResult)
        (goValue _binaryOpArg1)
        (goValue _binaryOpArg2)

    goLoad :: InstrLoad -> ByteString
    goLoad InstrLoad {..} =
      quad
        "OpLoad"
        (show _instrLoadDest)
        (show _instrLoadSrc)
        (show _instrLoadOffset)

    goStore :: InstrStore -> ByteString
    goStore InstrStore {..} =
      quad
        "OpStore"
        (show _instrStoreDest)
        (show _instrStoreOffset)
        (goValue _instrStoreValue)

    goMove :: InstrMove -> ByteString
    goMove InstrMove {..} =
      quad
        "OpMove"
        (show _instrMoveDest)
        (goValue _instrMoveValue)
        "0"

    goHalt :: ByteString
    goHalt = quad "OpHalt" "0" "0" "0"

    goAlloc :: InstrAlloc -> ByteString
    goAlloc InstrAlloc {..} =
      quad
        "OpAlloc"
        (show _instrAllocDest)
        (goValue _instrAllocSize)
        "0"

    goPush :: InstrPush -> ByteString
    goPush InstrPush {..} =
      quad
        "OpPush"
        "0"
        (goValue _instrPushValue)
        "0"

    goPop :: InstrPop -> ByteString
    goPop InstrPop {..} =
      quad
        "OpPop"
        (show _instrPopDest)
        "0"
        "0"

    goJump :: InstrJump -> ByteString
    goJump InstrJump {..} =
      quad
        "OpJump"
        "0"
        (goValue _instrJumpDest)
        "0"

    goJumpOnZero :: InstrJumpOnZero -> ByteString
    goJumpOnZero InstrJumpOnZero {..} =
      quad
        "OpJumpOnZero"
        (show _instrJumpOnZeroReg)
        (goValue _instrJumpOnZeroDest)
        "0"

    goValue :: Value -> ByteString
    goValue = \case
      Const x -> "Cst " <> show x
      RegRef x -> "Reg " <> show x
      LabelRef {} -> impossible

    goOpcode :: Opcode -> ByteString
    goOpcode = \case
      OpIntAdd -> "OpIntAdd"
      OpIntSub -> "OpIntSub"
      OpIntMul -> "OpIntMul"
      OpIntDiv -> "OpIntDiv"
      OpIntMod -> "OpIntMod"
      OpIntLt -> "OpIntLt"
      OpIntEq -> "OpIntEq"

vampirPrelude :: Int -> Options -> ByteString
vampirPrelude regsNum opts =
  "def integerBits = "
    <> show (opts ^. optIntegerBits)
    <> ";\n"
    <> "def regsNum = "
    <> show regsNum
    <> ";\n"
    <> "def stackSize = "
    <> show (opts ^. optStackSize)
    <> ";\n"
    <> "def heapSize = "
    <> show (opts ^. optHeapSize)
    <> ";\n"
    <> $(FE.makeRelativeToProject "runtime/src/vampir/stdlib.pir" >>= FE.embedFile)
    <> ";\n"
    <> $(FE.makeRelativeToProject "runtime/src/vampir/vm.pir" >>= FE.embedFile)
    <> "\n"
