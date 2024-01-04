module Juvix.Compiler.Casm.Interpreter where

import Control.Monad.ST
import Data.HashMap.Strict qualified as HashMap
import Data.Vector qualified as Vec
import Data.Vector.Mutable qualified as MV
import Juvix.Compiler.Casm.Data.LabelInfo
import Juvix.Compiler.Casm.Language hiding (ap)

-- | Runs Cairo Assembly. Returns the value of `[ap - 1]` at program exit.
runCode :: LabelInfo -> [Instruction] -> Integer
runCode (LabelInfo labelInfo) instrs0 = runST goCode
  where
    instrs :: Vec.Vector Instruction
    instrs = Vec.fromList instrs0

    initialMemSize :: Int
    initialMemSize = 1024

    goCode :: ST s Integer
    goCode = do
      mem <- MV.replicate initialMemSize 0
      go 0 0 0 mem

    go ::
      Address ->
      Address ->
      Address ->
      MV.MVector s Integer ->
      ST s Integer
    go pc ap fp mem
      | Vec.length instrs <= pc =
          MV.read mem (ap - 1)
      | otherwise =
          case instrs Vec.! pc of
            Assign x -> goAssign x pc ap fp mem
            Binop x -> goBinop x pc ap fp mem
            Load x -> goLoad x pc ap fp mem
            Jump x -> goJump x pc ap fp mem
            JumpIf x -> goJumpIf x pc ap fp mem
            Call x -> goCall x pc ap fp mem
            Return -> goReturn pc ap fp mem
            Alloc x -> goAlloc x pc ap fp mem
            Label {} -> go (pc + 1) ap fp mem

    readReg :: Address -> Address -> Reg -> Address
    readReg ap fp = \case
      Ap -> ap
      Fp -> fp

    writeMem :: MV.MVector s Integer -> Address -> Integer -> ST s (MV.MVector s Integer)
    writeMem mem addr v = do
      let len = MV.length mem
      mem' <-
        if
            | addr >= len ->
                MV.grow mem (max (addr + initialMemSize - len) len)
            | otherwise ->
                return mem
      MV.write mem' addr v
      return mem'

    writeMemRef :: Address -> Address -> MV.MVector s Integer -> MemRef -> Integer -> ST s (MV.MVector s Integer)
    writeMemRef ap fp mem MemRef {..} v = do
      let r = readReg ap fp _memRefReg
          off :: Int = fromIntegral _memRefOff
      writeMem mem (r + off) v

    readMemRef :: Address -> Address -> MV.MVector s Integer -> MemRef -> ST s Integer
    readMemRef ap fp mem MemRef {..} = do
      let r = readReg ap fp _memRefReg
          off :: Int = fromIntegral _memRefOff
      MV.read mem (r + off)

    readLabel :: LabelRef -> Integer
    readLabel LabelRef {..} =
      fromIntegral $ fromJust $ HashMap.lookup _labelRefSymbol labelInfo

    readValue :: Address -> Address -> MV.MVector s Integer -> Value -> ST s Integer
    readValue ap fp mem = \case
      Imm v -> return v
      Ref r -> readMemRef ap fp mem r
      Lab l -> return $ readLabel l

    goAssign :: InstrAssign -> Address -> Address -> Address -> MV.MVector s Integer -> ST s Integer
    goAssign InstrAssign {..} pc ap fp mem = do
      v <- readValue ap fp mem _instrAssignValue
      mem' <- writeMemRef ap fp mem _instrAssignResult v
      go (pc + 1) (ap + fromEnum _instrAssignIncAp) fp mem'

    goBinop :: InstrBinop -> Address -> Address -> Address -> MV.MVector s Integer -> ST s Integer
    goBinop InstrBinop {..} pc ap fp mem = do
      v1 <- readMemRef ap fp mem _instrBinopArg1
      v2 <- readValue ap fp mem _instrBinopArg2
      let v = goOp v1 v2 _instrBinopOpcode
      mem' <- writeMemRef ap fp mem _instrBinopResult v
      go (pc + 1) (ap + fromEnum _instrBinopIncAp) fp mem'
      where
        goOp :: Integer -> Integer -> Opcode -> Integer
        goOp x y = \case
          FieldAdd -> x + y
          FieldSub -> x - y
          FieldMul -> x * y

    goLoad :: InstrLoad -> Address -> Address -> Address -> MV.MVector s Integer -> ST s Integer
    goLoad InstrLoad {..} pc ap fp mem = do
      src <- readMemRef ap fp mem _instrLoadSrc
      let off :: Int = fromIntegral _instrLoadOff
          addr :: Int = fromInteger src + off
      v <- MV.read mem addr
      mem' <- writeMemRef ap fp mem _instrLoadResult v
      go (pc + 1) (ap + fromEnum _instrLoadIncAp) fp mem'

    goJump :: InstrJump -> Address -> Address -> Address -> MV.MVector s Integer -> ST s Integer
    goJump InstrJump {..} _ ap fp mem = do
      tgt <- readValue ap fp mem _instrJumpTarget
      go (fromInteger tgt) (ap + fromEnum _instrJumpIncAp) fp mem

    goJumpIf :: InstrJumpIf -> Address -> Address -> Address -> MV.MVector s Integer -> ST s Integer
    goJumpIf InstrJumpIf {..} pc ap fp mem = do
      tgt <- readValue ap fp mem _instrJumpIfTarget
      v <- readValue ap fp mem _instrJumpIfValue
      go (if v /= 0 then fromInteger tgt else pc + 1) (ap + fromEnum _instrJumpIfIncAp) fp mem

    goCall :: InstrCall -> Address -> Address -> Address -> MV.MVector s Integer -> ST s Integer
    goCall InstrCall {..} pc ap fp mem = do
      tgt <- readValue ap fp mem _instrCallTarget
      mem' <- writeMem mem ap (fromIntegral fp)
      mem'' <- writeMem mem' (ap + 1) (fromIntegral pc + 1)
      go (fromInteger tgt) (ap + 2) (ap + 2) mem''

    goReturn :: Address -> Address -> Address -> MV.MVector s Integer -> ST s Integer
    goReturn _ ap fp mem = do
      pc' <- MV.read mem (fp - 1)
      fp' <- MV.read mem (fp - 2)
      go (fromInteger pc') ap (fromInteger fp') mem

    goAlloc :: InstrAlloc -> Address -> Address -> Address -> MV.MVector s Integer -> ST s Integer
    goAlloc InstrAlloc {..} pc ap fp mem =
      go (pc + 1) (ap + _instrAllocSize) fp mem
