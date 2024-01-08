module Juvix.Compiler.Casm.Interpreter
  ( module Juvix.Compiler.Casm.Interpreter,
    module Juvix.Compiler.Casm.Language,
    module Juvix.Compiler.Casm.Data.LabelInfo,
  )
where

import Control.Exception qualified as Exception
import Control.Monad.ST
import Data.HashMap.Strict qualified as HashMap
import Data.Vector qualified as Vec
import Data.Vector.Mutable qualified as MV
import GHC.IO qualified as GHC
import Juvix.Compiler.Casm.Data.LabelInfo
import Juvix.Compiler.Casm.Error
import Juvix.Compiler.Casm.Interpreter.Error
import Juvix.Compiler.Casm.Language hiding (ap)

type Memory s = MV.MVector s (Maybe Integer)

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
      mem <- MV.replicate initialMemSize Nothing
      go 0 0 0 mem

    go ::
      Address ->
      Address ->
      Address ->
      Memory s ->
      ST s Integer
    go pc ap fp mem
      | Vec.length instrs <= pc =
          readMem mem (ap - 1)
      | otherwise =
          case instrs Vec.! pc of
            Assign x -> goAssign x pc ap fp mem
            ExtraBinop x -> goExtraBinop x pc ap fp mem
            Jump x -> goJump x pc ap fp mem
            JumpIf x -> goJumpIf x pc ap fp mem
            JumpRel x -> goJumpRel x pc ap fp mem
            Call x -> goCall x pc ap fp mem
            Return -> goReturn pc ap fp mem
            Alloc x -> goAlloc x pc ap fp mem
            Trace x -> goTrace x pc ap fp mem
            Label {} -> go (pc + 1) ap fp mem

    readReg :: Address -> Address -> Reg -> Address
    readReg ap fp = \case
      Ap -> ap
      Fp -> fp

    readMem :: Memory s -> Address -> ST s Integer
    readMem mem addr = do
      mv <- MV.read mem addr
      case mv of
        Just v -> return v
        Nothing -> throwRunError ("reading uninitialized memory at address " <> show addr)

    writeMem :: Memory s -> Address -> Integer -> ST s (Memory s)
    writeMem mem addr v = do
      let len = MV.length mem
      mem' <-
        if
            | addr >= len ->
                MV.grow mem (max (addr + initialMemSize - len) len)
            | otherwise ->
                return mem
      MV.write mem' addr (Just v)
      return mem'

    writeMemRef :: Address -> Address -> Memory s -> MemRef -> Integer -> ST s (Memory s)
    writeMemRef ap fp mem MemRef {..} v = do
      let r = readReg ap fp _memRefReg
          off :: Int = fromIntegral _memRefOff
      writeMem mem (r + off) v

    readMemRef :: Address -> Address -> Memory s -> MemRef -> ST s Integer
    readMemRef ap fp mem MemRef {..} = do
      let r = readReg ap fp _memRefReg
          off :: Int = fromIntegral _memRefOff
      readMem mem (r + off)

    readLabel :: LabelRef -> Integer
    readLabel LabelRef {..} =
      fromIntegral $ fromMaybe (throwRunError "invalid label") $ HashMap.lookup _labelRefSymbol labelInfo

    readValue :: Address -> Address -> Memory s -> Value -> ST s Integer
    readValue ap fp mem = \case
      Imm v -> return v
      Ref r -> readMemRef ap fp mem r
      Lab l -> return $ readLabel l

    readLoadValue :: Address -> Address -> Memory s -> LoadValue -> ST s Integer
    readLoadValue ap fp mem LoadValue {..} = do
      src <- readMemRef ap fp mem _loadValueSrc
      let off :: Int = fromIntegral _loadValueOff
          addr :: Int = fromInteger src + off
      readMem mem addr

    readBinopValue :: Address -> Address -> Memory s -> BinopValue -> ST s Integer
    readBinopValue ap fp mem BinopValue {..} = do
      v1 <- readMemRef ap fp mem _binopValueArg1
      v2 <- readValue ap fp mem _binopValueArg2
      return $ goOp v1 v2 _binopValueOpcode
      where
        goOp :: Integer -> Integer -> Opcode -> Integer
        goOp x y = \case
          FieldAdd -> x + y
          FieldMul -> x * y

    readRValue :: Address -> Address -> Memory s -> RValue -> ST s Integer
    readRValue ap fp mem = \case
      Val x -> readValue ap fp mem x
      Load x -> readLoadValue ap fp mem x
      Binop x -> readBinopValue ap fp mem x

    goAssign :: InstrAssign -> Address -> Address -> Address -> Memory s -> ST s Integer
    goAssign InstrAssign {..} pc ap fp mem = do
      v <- readRValue ap fp mem _instrAssignValue
      mem' <- writeMemRef ap fp mem _instrAssignResult v
      go (pc + 1) (ap + fromEnum _instrAssignIncAp) fp mem'

    goExtraBinop :: InstrExtraBinop -> Address -> Address -> Address -> Memory s -> ST s Integer
    goExtraBinop InstrExtraBinop {..} pc ap fp mem = do
      v1 <- readMemRef ap fp mem _instrExtraBinopArg1
      v2 <- readValue ap fp mem _instrExtraBinopArg2
      let v = goOp v1 v2 _instrExtraBinopOpcode
      mem' <- writeMemRef ap fp mem _instrExtraBinopResult v
      go (pc + 1) (ap + fromEnum _instrExtraBinopIncAp) fp mem'
      where
        goOp :: Integer -> Integer -> ExtraOpcode -> Integer
        goOp x y = \case
          FieldSub -> x - y

    goJump :: InstrJump -> Address -> Address -> Address -> Memory s -> ST s Integer
    goJump InstrJump {..} _ ap fp mem = do
      tgt <- readValue ap fp mem _instrJumpTarget
      go (fromInteger tgt) (ap + fromEnum _instrJumpIncAp) fp mem

    goJumpIf :: InstrJumpIf -> Address -> Address -> Address -> Memory s -> ST s Integer
    goJumpIf InstrJumpIf {..} pc ap fp mem = do
      tgt <- readValue ap fp mem _instrJumpIfTarget
      v <- readMemRef ap fp mem _instrJumpIfValue
      go (if v /= 0 then fromInteger tgt else pc + 1) (ap + fromEnum _instrJumpIfIncAp) fp mem

    goJumpRel :: InstrJumpRel -> Address -> Address -> Address -> Memory s -> ST s Integer
    goJumpRel InstrJumpRel {..} pc ap fp mem = do
      tgt <- readRValue ap fp mem _instrJumpRelTarget
      go (pc + fromInteger tgt) (ap + fromEnum _instrJumpRelIncAp) fp mem

    goCall :: InstrCall -> Address -> Address -> Address -> Memory s -> ST s Integer
    goCall InstrCall {..} pc ap fp mem = do
      tgt <- readValue ap fp mem _instrCallTarget
      mem' <- writeMem mem ap (fromIntegral fp)
      mem'' <- writeMem mem' (ap + 1) (fromIntegral pc + 1)
      go (fromInteger tgt) (ap + 2) (ap + 2) mem''

    goReturn :: Address -> Address -> Address -> Memory s -> ST s Integer
    goReturn _ ap fp mem = do
      pc' <- readMem mem (fp - 1)
      fp' <- readMem mem (fp - 2)
      go (fromInteger pc') ap (fromInteger fp') mem

    goAlloc :: InstrAlloc -> Address -> Address -> Address -> Memory s -> ST s Integer
    goAlloc InstrAlloc {..} pc ap fp mem = do
      v <- readRValue ap fp mem _instrAllocSize
      go (pc + 1) (ap + fromInteger v) fp mem

    goTrace :: InstrTrace -> Address -> Address -> Address -> Memory s -> ST s Integer
    goTrace InstrTrace {..} pc ap fp mem = do
      v <- readRValue ap fp mem _instrTraceValue
      GHC.unsafePerformIO (print v >> return (pure ()))
      go (pc + 1) ap fp mem

catchRunErrorIO :: a -> IO (Either CasmError a)
catchRunErrorIO a =
  Exception.catch
    (Exception.evaluate a >>= \a' -> return $ Right a')
    (\(ex :: RunError) -> return $ Left (toCasmError ex))

toCasmError :: RunError -> CasmError
toCasmError (RunError {..}) =
  CasmError
    { _casmErrorMsg = "runtime error: " <> _runErrorMsg,
      _casmErrorLoc = Nothing
    }
