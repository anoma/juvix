module Juvix.Compiler.VM.Interpreter where

import Control.Monad.ST
import Data.Vector qualified as Vec
import Data.Vector.Unboxed.Mutable qualified as MV
import Juvix.Compiler.VM.Extra.Utils
import Juvix.Compiler.VM.Language

-- | Runs VM bytecode. Returns the contents of r0 at the end of execution.
runCode :: [Instruction] -> Int
runCode instrs0 = runST goCode
  where
    instrs :: Vec.Vector Instruction
    instrs = Vec.fromList instrs0
    heapSize :: Int
    heapSize = 1024
    stackSize :: Int
    stackSize = 1024
    regsNum :: Int
    regsNum = maximum (map maxInstrReg instrs0) + 1

    goCode :: ST s Int
    goCode = do
      heap <- MV.replicate heapSize 0
      stack <- MV.replicate stackSize 0
      regs <- MV.replicate regsNum 0
      go 0 0 1 regs stack heap
      MV.read regs 0

    go ::
      Int ->
      Int ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    go pc sp hp regs stack heap =
      case instrs Vec.! pc of
        Binop x -> goBinop x pc sp hp regs stack heap
        Load x -> goLoad x pc sp hp regs stack heap
        Store x -> goStore x pc sp hp regs stack heap
        Move x -> goMove x pc sp hp regs stack heap
        Halt -> return ()
        Alloc x -> goAlloc x pc sp hp regs stack heap
        Push x -> goPush x pc sp hp regs stack heap
        Pop x -> goPop x pc sp hp regs stack heap
        Jump x -> goJump x pc sp hp regs stack heap
        JumpOnZero x -> goJumpOnZero x pc sp hp regs stack heap
        Label {} -> impossible

    readValue :: MV.MVector s Int -> Value -> ST s Int
    readValue regs = \case
      Const x -> return x
      RegRef r -> MV.read regs r
      LabelRef {} -> impossible

    goBinop ::
      BinaryOp ->
      Int ->
      Int ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goBinop BinaryOp {..} pc sp hp regs stack heap = do
      val1 <- readValue regs _binaryOpArg1
      val2 <- readValue regs _binaryOpArg2
      MV.write regs _binaryOpResult (computeBinop val1 val2)
      go (pc + 1) sp hp regs stack heap
      where
        computeBinop :: Int -> Int -> Int
        computeBinop v1 v2 = case _binaryOpCode of
          OpIntAdd -> v1 + v2
          OpIntSub -> v1 - v2
          OpIntMul -> v1 * v2
          OpIntDiv -> v1 `div` v2
          OpIntMod -> v1 `rem` v2
          OpIntLt -> if v1 < v2 then 1 else 0
          OpIntEq -> if v1 == v2 then 1 else 0

    goLoad ::
      InstrLoad ->
      Int ->
      Int ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goLoad InstrLoad {..} pc sp hp regs stack heap = do
      src <- MV.read regs _instrLoadSrc
      v <- MV.read heap (src + _instrLoadOffset)
      MV.write regs _instrLoadDest v
      go (pc + 1) sp hp regs stack heap

    goStore ::
      InstrStore ->
      Int ->
      Int ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goStore InstrStore {..} pc sp hp regs stack heap = do
      dest <- MV.read regs _instrStoreDest
      v <- readValue regs _instrStoreValue
      MV.write heap (dest + _instrStoreOffset) v
      go (pc + 1) sp hp regs stack heap

    goMove ::
      InstrMove ->
      Int ->
      Int ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goMove InstrMove {..} pc sp hp regs stack heap = do
      v <- readValue regs _instrMoveValue
      MV.write regs _instrMoveDest v
      go (pc + 1) sp hp regs stack heap

    goAlloc ::
      InstrAlloc ->
      Int ->
      Int ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goAlloc InstrAlloc {..} pc sp hp regs stack heap = do
      v <- readValue regs _instrAllocSize
      MV.write regs _instrAllocDest hp
      go (pc + 1) sp (hp + v) regs stack heap

    goPush ::
      InstrPush ->
      Int ->
      Int ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goPush InstrPush {..} pc sp hp regs stack heap = do
      v <- readValue regs _instrPushValue
      MV.write stack sp v
      go (pc + 1) (sp + 1) hp regs stack heap

    goPop ::
      InstrPop ->
      Int ->
      Int ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goPop InstrPop {..} pc sp hp regs stack heap = do
      v <- MV.read stack (sp - 1)
      MV.write regs _instrPopDest v
      go (pc + 1) (sp - 1) hp regs stack heap

    goJump ::
      InstrJump ->
      Int ->
      Int ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goJump InstrJump {..} _ sp hp regs stack heap = do
      addr <- readValue regs _instrJumpDest
      go addr sp hp regs stack heap

    goJumpOnZero ::
      InstrJumpOnZero ->
      Int ->
      Int ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goJumpOnZero InstrJumpOnZero {..} pc sp hp regs stack heap = do
      addr <- readValue regs _instrJumpOnZeroDest
      v <- MV.read regs _instrJumpOnZeroReg
      go (if v == 0 then addr else pc + 1) sp hp regs stack heap
