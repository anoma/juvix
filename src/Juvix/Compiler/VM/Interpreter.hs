module Juvix.Compiler.VM.Interpreter where

import Control.Monad.ST
import Data.HashMap.Strict qualified as HashMap
import Data.Vector qualified as Vec
import Data.Vector.Unboxed.Mutable qualified as MV
import Juvix.Compiler.VM.Extra.Utils
import Juvix.Compiler.VM.Language
import Juvix.Compiler.VM.Options

-- | Runs VM bytecode. Returns the contents of r0 at the end of execution.
runCode :: Options -> HashMap Text Int -> [Instruction] -> Int
runCode opts vars instrs0 = runST goCode
  where
    instrs :: Vec.Vector Instruction
    instrs = Vec.fromList instrs0
    heapSize :: Int
    heapSize = opts ^. optHeapSize
    stackSize :: Int
    stackSize = max 1 (opts ^. optStackSize)
    memSize :: Int
    memSize = stackSize + heapSize
    regsNum :: Int
    regsNum = max 3 (computeRegsNum instrs0)

    goCode :: ST s Int
    goCode = do
      mem <- MV.replicate memSize 0
      regs <- MV.replicate regsNum 0
      MV.write regs 1 stackSize
      go 0 regs mem
      MV.read regs 2

    go ::
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    go pc regs mem =
      case instrs Vec.! pc of
        Binop x -> goBinop x pc regs mem
        Load x -> goLoad x pc regs mem
        Store x -> goStore x pc regs mem
        Move x -> goMove x pc regs mem
        Halt -> return ()
        Jump x -> goJump x pc regs mem
        JumpOnZero x -> goJumpOnZero x pc regs mem
        Label {} -> impossible

    readValue :: MV.MVector s Int -> Value -> ST s Int
    readValue regs = \case
      Const x -> return x
      RegRef r -> MV.read regs r
      LabelRef {} -> impossible
      VarRef x ->
        return $ fromMaybe (error ("unbound variable: " <> x)) $ HashMap.lookup x vars

    goBinop ::
      BinaryOp ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goBinop BinaryOp {..} pc regs mem = do
      val1 <- readValue regs _binaryOpArg1
      val2 <- readValue regs _binaryOpArg2
      MV.write regs _binaryOpResult (computeBinop val1 val2)
      go (pc + 1) regs mem
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
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goLoad InstrLoad {..} pc regs mem = do
      src <- MV.read regs _instrLoadSrc
      v <- MV.read mem (src + _instrLoadOffset)
      MV.write regs _instrLoadDest v
      go (pc + 1) regs mem

    goStore ::
      InstrStore ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goStore InstrStore {..} pc regs mem = do
      dest <- MV.read regs _instrStoreDest
      v <- readValue regs _instrStoreValue
      MV.write mem (dest + _instrStoreOffset) v
      go (pc + 1) regs mem

    goMove ::
      InstrMove ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goMove InstrMove {..} pc regs mem = do
      v <- readValue regs _instrMoveValue
      MV.write regs _instrMoveDest v
      go (pc + 1) regs mem

    goJump ::
      InstrJump ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goJump InstrJump {..} _ regs mem = do
      addr <- readValue regs _instrJumpDest
      go addr regs mem

    goJumpOnZero ::
      InstrJumpOnZero ->
      Int ->
      MV.MVector s Int ->
      MV.MVector s Int ->
      ST s ()
    goJumpOnZero InstrJumpOnZero {..} pc regs mem = do
      addr <- readValue regs _instrJumpOnZeroDest
      v <- MV.read regs _instrJumpOnZeroReg
      go (if v == 0 then addr else pc + 1) regs mem
