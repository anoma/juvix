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
import Juvix.Compiler.Casm.Data.InputInfo
import Juvix.Compiler.Casm.Data.LabelInfo
import Juvix.Compiler.Casm.Error
import Juvix.Compiler.Casm.Interpreter.Error
import Juvix.Compiler.Casm.Language hiding (ap)
import Juvix.Data.Field

type Memory s = MV.MVector s (Maybe FField)

runCode :: LabelInfo -> [Instruction] -> FField
runCode = hRunCode (InputInfo mempty) stderr

-- | Runs Cairo Assembly. Returns the value of `[ap - 1]` at program exit.
hRunCode :: InputInfo -> Handle -> LabelInfo -> [Instruction] -> FField
hRunCode inputInfo hout (LabelInfo labelInfo) instrs0 = runST goCode
  where
    instrs :: Vec.Vector Instruction
    instrs = Vec.fromList instrs0

    initialMemSize :: Int
    initialMemSize = 1024

    fsize :: Natural
    fsize = cairoFieldSize

    goCode :: ST s FField
    goCode = do
      mem <- MV.replicate initialMemSize Nothing
      go 0 0 0 mem

    go ::
      Address ->
      Address ->
      Address ->
      Memory s ->
      ST s FField
    go pc ap fp mem
      | Vec.length instrs == pc =
          goFinish ap mem
      | Vec.length instrs < pc =
          throwRunError ("invalid program counter: " <> show pc)
      | otherwise =
          case instrs Vec.! pc of
            Assign x -> goAssign x pc ap fp mem
            ExtraBinop x -> goExtraBinop x pc ap fp mem
            Jump x -> goJump x pc ap fp mem
            JumpIf x -> goJumpIf x pc ap fp mem
            Call x -> goCall x pc ap fp mem
            Return -> goReturn pc ap fp mem
            Alloc x -> goAlloc x pc ap fp mem
            Trace x -> goTrace x pc ap fp mem
            Hint x -> goHint x pc ap fp mem
            Label {} -> go (pc + 1) ap fp mem
            Nop -> go (pc + 1) ap fp mem

    checkGaps :: forall s. Memory s -> ST s ()
    checkGaps mem = goGaps False 0
      where
        len :: Int = MV.length mem

        goGaps :: Bool -> Int -> ST s ()
        goGaps wasNothing i
          | i < len = do
              mv <- MV.read mem i
              case mv of
                Nothing -> goGaps True (i + 1)
                Just {}
                  | wasNothing ->
                      throwRunError ("non-continuous memory use at address " <> show i)
                  | otherwise ->
                      goGaps False (i + 1)
          | otherwise = return ()

    readReg :: Address -> Address -> Reg -> Address
    readReg ap fp = \case
      Ap -> ap
      Fp -> fp

    readMem :: Memory s -> Address -> ST s FField
    readMem mem addr = do
      mv <- MV.read mem addr
      case mv of
        Just v -> return v
        Nothing -> throwRunError ("reading uninitialized memory at address " <> show addr)

    writeMem :: Memory s -> Address -> FField -> ST s (Memory s)
    writeMem mem addr v = do
      let len = MV.length mem
      mem' <-
        if
            | addr >= len -> do
                mem' <- MV.grow mem (max (addr + initialMemSize - len) len)
                mapM_ (\i -> MV.write mem' i Nothing) [len .. MV.length mem' - 1]
                return mem'
            | otherwise ->
                return mem
      whenJustM (MV.read mem' addr) $
        throwRunError ("double memory write at address " <> show addr)
      MV.write mem' addr (Just v)
      return mem'

    writeMemRef :: Address -> Address -> Memory s -> MemRef -> FField -> ST s (Memory s)
    writeMemRef ap fp mem MemRef {..} v = do
      let r = readReg ap fp _memRefReg
          off :: Int = fromIntegral _memRefOff
      writeMem mem (r + off) v

    readMemRef :: Address -> Address -> Memory s -> MemRef -> ST s FField
    readMemRef ap fp mem MemRef {..} = do
      let r = readReg ap fp _memRefReg
          off :: Int = fromIntegral _memRefOff
      readMem mem (r + off)

    readLabel :: LabelRef -> FField
    readLabel LabelRef {..} =
      fieldFromInteger fsize $
        fromIntegral $
          fromMaybe (throwRunError "invalid label") $
            HashMap.lookup _labelRefSymbol labelInfo

    readValue' :: Bool -> Address -> Address -> Address -> Memory s -> Value -> ST s FField
    readValue' isRel pc ap fp mem = \case
      Imm v -> return $ fieldFromInteger fsize v
      Ref r -> readMemRef ap fp mem r
      Lab l -> return $ if isRel then fieldSub lab (fieldFromInteger fsize (fromIntegral pc)) else lab
        where
          lab = readLabel l

    readValue :: Address -> Address -> Memory s -> Value -> ST s FField
    readValue = readValue' False 0

    readLoadValue :: Address -> Address -> Memory s -> LoadValue -> ST s FField
    readLoadValue ap fp mem LoadValue {..} = do
      src <- readMemRef ap fp mem _loadValueSrc
      let off :: Int = fromIntegral _loadValueOff
          addr :: Int = fromInteger (fieldToInteger src) + off
      readMem mem addr

    readBinopValue :: Address -> Address -> Memory s -> BinopValue -> ST s FField
    readBinopValue ap fp mem BinopValue {..} = do
      v1 <- readMemRef ap fp mem _binopValueArg1
      v2 <- readValue ap fp mem _binopValueArg2
      return $ goOp v1 v2 _binopValueOpcode
      where
        goOp :: FField -> FField -> Opcode -> FField
        goOp x y = \case
          FieldAdd -> fieldAdd x y
          FieldMul -> fieldMul x y

    readRValue' :: Bool -> Address -> Address -> Address -> Memory s -> RValue -> ST s FField
    readRValue' isRel pc ap fp mem = \case
      Val x -> readValue' isRel pc ap fp mem x
      Load x -> readLoadValue ap fp mem x
      Binop x -> readBinopValue ap fp mem x

    readRValue :: Address -> Address -> Memory s -> RValue -> ST s FField
    readRValue = readRValue' False 0

    goAssign :: InstrAssign -> Address -> Address -> Address -> Memory s -> ST s FField
    goAssign InstrAssign {..} pc ap fp mem = do
      v <- readRValue ap fp mem _instrAssignValue
      mem' <- writeMemRef ap fp mem _instrAssignResult v
      go (pc + 1) (ap + fromEnum _instrAssignIncAp) fp mem'

    goExtraBinop :: InstrExtraBinop -> Address -> Address -> Address -> Memory s -> ST s FField
    goExtraBinop InstrExtraBinop {..} pc ap fp mem = do
      v1 <- readMemRef ap fp mem _instrExtraBinopArg1
      v2 <- readValue ap fp mem _instrExtraBinopArg2
      let v = goOp v1 v2 _instrExtraBinopOpcode
      mem' <- writeMemRef ap fp mem _instrExtraBinopResult v
      go (pc + 1) (ap + fromEnum _instrExtraBinopIncAp) fp mem'
      where
        goOp :: FField -> FField -> ExtraOpcode -> FField
        goOp x y = \case
          FieldSub -> fieldSub x y
          FieldDiv -> fieldDiv x y
          IntAdd -> fieldAdd x y
          IntSub -> fieldSub x y
          IntMul -> fieldMul x y
          IntDiv -> fieldFromInteger fsize (fieldToInt x `quot` fieldToInt y)
          IntMod -> fieldFromInteger fsize (fieldToInt x `rem` fieldToInt y)
          IntLt ->
            fieldFromInteger fsize $
              if fieldToInt x < fieldToInt y then 0 else 1

        fieldToInt :: FField -> Integer
        fieldToInt f
          | v < fromIntegral fsize `div` 2 = v
          | otherwise = v - fromIntegral fsize
          where
            v = fieldToInteger f

    goJump :: InstrJump -> Address -> Address -> Address -> Memory s -> ST s FField
    goJump InstrJump {..} pc ap fp mem = do
      tgt <- readRValue' _instrJumpRel pc ap fp mem _instrJumpTarget
      let off = if _instrJumpRel then pc else 0
      go (off + fromInteger (fieldToInteger tgt)) (ap + fromEnum _instrJumpIncAp) fp mem

    goJumpIf :: InstrJumpIf -> Address -> Address -> Address -> Memory s -> ST s FField
    goJumpIf InstrJumpIf {..} pc ap fp mem = do
      tgt <- readValue' True pc ap fp mem _instrJumpIfTarget
      v <- readMemRef ap fp mem _instrJumpIfValue
      go (if fieldToInteger v /= 0 then pc + fromInteger (fieldToInteger tgt) else pc + 1) (ap + fromEnum _instrJumpIfIncAp) fp mem

    goCall :: InstrCall -> Address -> Address -> Address -> Memory s -> ST s FField
    goCall InstrCall {..} pc ap fp mem = do
      tgt <- readValue' _instrCallRel pc ap fp mem _instrCallTarget
      mem' <- writeMem mem ap (fieldFromInteger fsize (fromIntegral fp))
      mem'' <- writeMem mem' (ap + 1) (fieldFromInteger fsize (fromIntegral pc + 1))
      let off = if _instrCallRel then pc else 0
      go (off + fromInteger (fieldToInteger tgt)) (ap + 2) (ap + 2) mem''

    goReturn :: Address -> Address -> Address -> Memory s -> ST s FField
    goReturn _ ap fp mem
      | fp == 0 =
          goFinish ap mem
      | otherwise = do
          pc' <- readMem mem (fp - 1)
          fp' <- readMem mem (fp - 2)
          go (fromInteger (fieldToInteger pc')) ap (fromInteger (fieldToInteger fp')) mem

    goAlloc :: InstrAlloc -> Address -> Address -> Address -> Memory s -> ST s FField
    goAlloc InstrAlloc {..} pc ap fp mem = do
      v <- readRValue ap fp mem _instrAllocSize
      go (pc + 1) (ap + fromInteger (fieldToInteger v)) fp mem

    goTrace :: InstrTrace -> Address -> Address -> Address -> Memory s -> ST s FField
    goTrace InstrTrace {..} pc ap fp mem = do
      v <- readRValue ap fp mem _instrTraceValue
      GHC.unsafePerformIO (hPrint hout v >> return (pure ()))
      go (pc + 1) ap fp mem

    goHint :: Hint -> Address -> Address -> Address -> Memory s -> ST s FField
    goHint hint pc ap fp mem = case hint of
      HintInput var -> do
        let val =
              fromMaybe (throwRunError "invalid input") $
                HashMap.lookup var (inputInfo ^. inputInfoMap)
        mem' <- writeMem mem ap val
        go (pc + 1) (ap + 1) fp mem'
      HintAlloc size -> do
        mem' <- writeMem mem ap (fieldFromInteger fsize (fromIntegral ap + 1))
        go (pc + 1) (ap + size + 1) fp mem'

    goFinish :: Address -> Memory s -> ST s FField
    goFinish ap mem = do
      checkGaps mem
      when (ap == 0) $
        throwRunError "nothing to return"
      readMem mem (ap - 1)

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
