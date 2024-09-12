module Juvix.Compiler.Casm.Validate where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Casm.Data.LabelInfo
import Juvix.Compiler.Casm.Error
import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Casm.Pretty

validate :: LabelInfo -> Code -> Either CasmError ()
validate labi instrs = mapM_ go instrs
  where
    go :: Instruction -> Either CasmError ()
    go = \case
      Assign x -> goAssign x
      ExtraBinop x -> goExtraBinop x
      Jump x -> goJump x
      JumpIf x -> goJumpIf x
      Call x -> goCall x
      Return -> return ()
      Alloc x -> goAlloc x
      Assert x -> goAssert x
      Trace x -> goTrace x
      Hint {} -> return ()
      Label {} -> return ()
      Nop -> return ()

    goLabelRef :: LabelRef -> Either CasmError ()
    goLabelRef l@LabelRef {..} = do
      unless (HashMap.member _labelRefSymbol (labi ^. labelInfoTable)) $
        Left $
          CasmError
            { _casmErrorMsg = "undefined label: " <> ppPrint l,
              _casmErrorLoc = Nothing
            }

    goValue :: Value -> Either CasmError ()
    goValue = \case
      Lab l -> goLabelRef l
      _ -> return ()

    goBinopValue :: BinopValue -> Either CasmError ()
    goBinopValue BinopValue {..} = goValue _binopValueArg2

    goRValue :: RValue -> Either CasmError ()
    goRValue = \case
      Val v -> goValue v
      Load {} -> return ()
      Binop x -> goBinopValue x

    goAssign :: InstrAssign -> Either CasmError ()
    goAssign InstrAssign {..} = goRValue _instrAssignValue

    goExtraBinop :: InstrExtraBinop -> Either CasmError ()
    goExtraBinop InstrExtraBinop {..} = goValue _instrExtraBinopArg2

    goJump :: InstrJump -> Either CasmError ()
    goJump InstrJump {..} = goRValue _instrJumpTarget

    goJumpIf :: InstrJumpIf -> Either CasmError ()
    goJumpIf InstrJumpIf {..} = goValue _instrJumpIfTarget

    goCall :: InstrCall -> Either CasmError ()
    goCall InstrCall {..} = goValue _instrCallTarget

    goAlloc :: InstrAlloc -> Either CasmError ()
    goAlloc InstrAlloc {..} = goRValue _instrAllocSize

    goTrace :: InstrTrace -> Either CasmError ()
    goTrace InstrTrace {..} = goRValue _instrTraceValue

    goAssert :: InstrAssert -> Either CasmError ()
    goAssert InstrAssert {} = return ()
