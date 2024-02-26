module Juvix.Compiler.Casm.Extra where

import Juvix.Compiler.Casm.Data.Limits
import Juvix.Compiler.Casm.Language

toOffset :: (Show a, Integral a) => a -> Offset
toOffset x
  | fromIntegral v == x = v
  | otherwise = error ("invalid offset: " <> show x)
  where
    v = fromIntegral x

mkExtraBinop :: ExtraOpcode -> MemRef -> MemRef -> Value -> Instruction
mkExtraBinop op res arg1 arg2 =
  ExtraBinop
    InstrExtraBinop
      { _instrExtraBinopOpcode = op,
        _instrExtraBinopResult = res,
        _instrExtraBinopArg1 = arg1,
        _instrExtraBinopArg2 = arg2,
        _instrExtraBinopIncAp = False
      }

mkNativeBinop :: Opcode -> MemRef -> MemRef -> Value -> Instruction
mkNativeBinop op res arg1 arg2 =
  Assign
    InstrAssign
      { _instrAssignResult = res,
        _instrAssignValue =
          Binop
            BinopValue
              { _binopValueOpcode = op,
                _binopValueArg1 = arg1,
                _binopValueArg2 = arg2
              },
        _instrAssignIncAp = False
      }

mkEq :: MemRef -> MemRef -> Value -> Instruction
mkEq res arg1 arg2 = mkExtraBinop FieldSub res arg1 arg2

mkIntLe :: MemRef -> MemRef -> Value -> [Instruction]
mkIntLe res arg1 arg2 = case arg2 of
  Imm v ->
    [mkExtraBinop IntLt res arg1 (Imm (v + 1))]
  Ref mref ->
    [inc, mkExtraBinop IntLt res (adjustAp 1 arg1) (Ref $ MemRef Ap (-1))]
    where
      inc =
        Assign
          InstrAssign
            { _instrAssignResult = MemRef Ap 0,
              _instrAssignValue =
                Binop
                  BinopValue
                    { _binopValueArg1 = mref,
                      _binopValueArg2 = Imm 1,
                      _binopValueOpcode = FieldAdd
                    },
              _instrAssignIncAp = True
            }
  Lab {} -> impossible

mkAssign :: MemRef -> RValue -> Instruction
mkAssign mr rv =
  Assign
    InstrAssign
      { _instrAssignResult = mr,
        _instrAssignValue = rv,
        _instrAssignIncAp = False
      }

mkAssignAp :: RValue -> Instruction
mkAssignAp v =
  Assign
    InstrAssign
      { _instrAssignResult = MemRef Ap 0,
        _instrAssignValue = v,
        _instrAssignIncAp = True
      }

mkOpArgsNum :: MemRef -> MemRef -> [Instruction]
mkOpArgsNum res v =
  [ mkAssignAp (Val $ Imm $ fromIntegral casmMaxFunctionArgs + 1),
    mkAssignAp (Load $ LoadValue (adjustAp 1 v) casmClosureArgsNumOffset),
    mkExtraBinop FieldSub res (MemRef Ap (-2)) (Ref $ MemRef Ap (-1))
  ]

adjustAp :: Int16 -> MemRef -> MemRef
adjustAp idx mr@MemRef {..} = case _memRefReg of
  Ap -> MemRef Ap (_memRefOff - idx)
  Fp -> mr
