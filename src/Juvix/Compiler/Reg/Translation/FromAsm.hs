module Juvix.Compiler.Reg.Translation.FromAsm where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable qualified as Asm
import Juvix.Compiler.Asm.Error qualified as Asm
import Juvix.Compiler.Asm.Extra.Recursors qualified as Asm
import Juvix.Compiler.Asm.Language qualified as Asm
import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Language

fromAsm :: Asm.InfoTable -> InfoTable
fromAsm tab =
  InfoTable
    { _infoFunctions = HashMap.map convertFun (tab ^. Asm.infoFunctions),
      _infoConstrs = HashMap.map convertConstr (tab ^. Asm.infoConstrs),
      _infoInductives = HashMap.map convertInductive (tab ^. Asm.infoInductives),
      _infoMainFunction = tab ^. Asm.infoMainFunction
    }
  where
    convertFun :: Asm.FunctionInfo -> FunctionInfo
    convertFun fi =
      FunctionInfo
        { _functionName = fi ^. Asm.functionName,
          _functionLocation = fi ^. Asm.functionLocation,
          _functionSymbol = fi ^. Asm.functionSymbol,
          _functionArgsNum = fi ^. Asm.functionArgsNum,
          _functionArgNames = fi ^. Asm.functionArgNames,
          _functionType = fi ^. Asm.functionType,
          _functionExtra = (),
          _functionCode = fromAsmFun tab fi
        }

    convertConstr :: Asm.ConstructorInfo -> ConstructorInfo
    convertConstr ci = ci

    convertInductive :: Asm.InductiveInfo -> InductiveInfo
    convertInductive ii = ii

fromAsmFun ::
  Asm.InfoTable ->
  Asm.FunctionInfo ->
  Code
fromAsmFun tab fi =
  case run $ runError $ Asm.recurseS sig (fi ^. Asm.functionCode) of
    Left err -> error (show err)
    Right code -> code
  where
    sig :: Asm.RecursorSig Asm.StackInfo (Error Asm.AsmError ': r) Instruction
    sig =
      Asm.RecursorSig
        { _recursorInfoTable = tab,
          _recurseInstr = fromAsmInstr fi tab,
          _recurseBranch = fromAsmBranch fi,
          _recurseCase = fromAsmCase fi tab,
          _recurseSave = fromAsmSave fi
        }

fromAsmInstr ::
  Asm.FunctionInfo ->
  Asm.InfoTable ->
  Asm.StackInfo ->
  Asm.CmdInstr ->
  Sem r Instruction
fromAsmInstr funInfo tab si Asm.CmdInstr {..} =
  case _cmdInstrInstruction of
    Asm.Binop op -> return $ mkBinop (mkOpcode op)
    Asm.ValShow -> return $ mkShow (mkVarRef VarGroupLocal (ntmps + n)) (VRef $ mkVarRef VarGroupLocal (ntmps + n))
    Asm.StrToInt -> return $ mkStrToInt (mkVarRef VarGroupLocal (ntmps + n)) (VRef $ mkVarRef VarGroupLocal (ntmps + n))
    Asm.Push val -> return $ mkAssign (mkVarRef VarGroupLocal (ntmps + n + 1)) (mkValue val)
    Asm.Pop -> return Nop
    Asm.Trace -> return $ Trace $ InstrTrace (VRef $ mkVarRef VarGroupLocal (ntmps + n))
    Asm.Dump -> return Dump
    Asm.Failure -> return $ Failure $ InstrFailure (VRef $ mkVarRef VarGroupLocal (ntmps + n))
    Asm.ArgsNum -> return $ mkArgsNum (mkVarRef VarGroupLocal (ntmps + n)) (VRef $ mkVarRef VarGroupLocal (ntmps + n))
    Asm.Prealloc x -> return $ mkPrealloc x
    Asm.AllocConstr tag -> return $ mkAlloc tag
    Asm.AllocClosure x -> return $ mkAllocClosure x
    Asm.ExtendClosure x -> return $ mkExtendClosure x
    Asm.Call x -> return $ mkCall False x
    Asm.TailCall x -> return $ mkCall True x
    Asm.CallClosures x -> return $ mkCallClosures False x
    Asm.TailCallClosures x -> return $ mkCallClosures True x
    Asm.Return ->
      return $ Return InstrReturn {_instrReturnValue = VRef $ mkVarRef VarGroupLocal ntmps}
  where
    extraInfo :: Asm.FunctionInfoExtra
    extraInfo = fromJust (funInfo ^. Asm.functionExtra)

    -- `n` is the index of the top of the value stack *before* executing the
    -- instruction
    n :: Int
    n = si ^. Asm.stackInfoValueStackHeight - 1

    -- `ntmps` is the number of temporary variables (= max temporary stack height)
    ntmps :: Int
    ntmps = extraInfo ^. Asm.functionMaxTempStackHeight

    -- Live variables *after* executing the instruction. `k` is the number of
    -- value stack cells that will be popped by the instruction. TODO: proper
    -- liveness analysis in JuvixAsm.
    liveVars :: Int -> [VarRef]
    liveVars k =
      map (mkVarRef VarGroupLocal) [0 .. si ^. Asm.stackInfoTempStackHeight - 1]
        ++ map (mkVarRef VarGroupLocal) [ntmps .. ntmps + n - k]
        ++ map (mkVarRef VarGroupArgs) [0 .. funInfo ^. Asm.functionArgsNum - 1]

    getArgs :: Int -> Int -> [Value]
    getArgs s k = map (\i -> VRef $ mkVarRef VarGroupLocal (ntmps + n - i)) [s .. (s + k - 1)]

    mkBinop :: Opcode -> Instruction
    mkBinop op =
      Binop
        ( BinaryOp
            { _binaryOpCode = op,
              _binaryOpResult = mkVarRef VarGroupLocal (ntmps + n - 1),
              _binaryOpArg1 = VRef $ mkVarRef VarGroupLocal (ntmps + n),
              _binaryOpArg2 = VRef $ mkVarRef VarGroupLocal (ntmps + n - 1)
            }
        )

    mkOpcode :: Asm.Opcode -> Opcode
    mkOpcode = \case
      Asm.IntAdd -> OpIntAdd
      Asm.IntSub -> OpIntSub
      Asm.IntMul -> OpIntMul
      Asm.IntDiv -> OpIntDiv
      Asm.IntMod -> OpIntMod
      Asm.IntLt -> OpIntLt
      Asm.IntLe -> OpIntLe
      Asm.ValEq -> OpEq
      Asm.StrConcat -> OpStrConcat

    mkShow :: VarRef -> Value -> Instruction
    mkShow tgt src = Show (InstrShow tgt src)

    mkStrToInt :: VarRef -> Value -> Instruction
    mkStrToInt tgt src = StrToInt (InstrStrToInt tgt src)

    mkAssign :: VarRef -> Value -> Instruction
    mkAssign tgt src = Assign (InstrAssign tgt src)

    mkArgsNum :: VarRef -> Value -> Instruction
    mkArgsNum tgt src = ArgsNum (InstrArgsNum tgt src)

    mkValue :: Asm.Value -> Value
    mkValue = \case
      Asm.Constant c -> Const c
      Asm.Ref mv -> case mv of
        Asm.DRef dref -> VRef $ mkVar dref
        Asm.ConstrRef Asm.Field {..} ->
          CRef $
            ConstrField
              { _constrFieldTag = _fieldTag,
                _constrFieldRef = mkVar _fieldRef,
                _constrFieldIndex = _fieldOffset,
                _constrFieldMemRep = ci ^. Asm.constructorRepresentation
              }
          where
            ci = fromMaybe impossible $ HashMap.lookup _fieldTag (tab ^. Asm.infoConstrs)

    mkVar :: Asm.DirectRef -> VarRef
    mkVar = \case
      Asm.ArgRef Asm.OffsetRef {..} -> VarRef VarGroupArgs _offsetRefOffset _offsetRefName
      Asm.TempRef Asm.RefTemp {..} -> VarRef VarGroupLocal (_refTempOffsetRef ^. Asm.offsetRefOffset) (_refTempOffsetRef ^. Asm.offsetRefName)

    mkPrealloc :: Asm.InstrPrealloc -> Instruction
    mkPrealloc Asm.InstrPrealloc {..} =
      Prealloc $
        InstrPrealloc
          { _instrPreallocWordsNum = _preallocWordsNum,
            _instrPreallocLiveVars = liveVars 0
          }

    mkAlloc :: Tag -> Instruction
    mkAlloc tag =
      Alloc $
        InstrAlloc
          { _instrAllocTag = tag,
            _instrAllocResult = mkVarRef VarGroupLocal (ntmps + m),
            _instrAllocArgs = getArgs 0 (ci ^. Asm.constructorArgsNum),
            _instrAllocMemRep = ci ^. Asm.constructorRepresentation
          }
      where
        ci = fromMaybe impossible $ HashMap.lookup tag (tab ^. Asm.infoConstrs)
        m = n - ci ^. Asm.constructorArgsNum + 1

    mkAllocClosure :: Asm.InstrAllocClosure -> Instruction
    mkAllocClosure Asm.InstrAllocClosure {..} =
      AllocClosure $
        InstrAllocClosure
          { _instrAllocClosureSymbol = fi ^. Asm.functionSymbol,
            _instrAllocClosureResult = mkVarRef VarGroupLocal (ntmps + m),
            _instrAllocClosureExpectedArgsNum = fi ^. Asm.functionArgsNum,
            _instrAllocClosureArgs = getArgs 0 _allocClosureArgsNum
          }
      where
        fi = fromMaybe impossible $ HashMap.lookup _allocClosureFunSymbol (tab ^. Asm.infoFunctions)
        m = n - _allocClosureArgsNum + 1

    mkExtendClosure :: Asm.InstrExtendClosure -> Instruction
    mkExtendClosure Asm.InstrExtendClosure {..} =
      ExtendClosure $
        InstrExtendClosure
          { _instrExtendClosureResult = mkVarRef VarGroupLocal (ntmps + m),
            _instrExtendClosureValue = mkVarRef VarGroupLocal (ntmps + n),
            _instrExtendClosureArgs = getArgs 1 _extendClosureArgsNum
          }
      where
        m = n - _extendClosureArgsNum

    mkCall :: Bool -> Asm.InstrCall -> Instruction
    mkCall isTail Asm.InstrCall {..}
      | not isTail =
          Call $
            InstrCall
              { _instrCallResult = mkVarRef VarGroupLocal (ntmps + m),
                _instrCallType = ct,
                _instrCallArgs = getArgs s _callArgsNum,
                _instrCallLiveVars = liveVars (_callArgsNum + s)
              }
      | otherwise =
          TailCall $
            InstrTailCall
              { _instrTailCallType = ct,
                _instrTailCallArgs = getArgs s _callArgsNum
              }
      where
        m = n - _callArgsNum - s + 1
        ct = case _callType of
          Asm.CallFun f -> CallFun f
          Asm.CallClosure -> CallClosure (mkVarRef VarGroupLocal (ntmps + n))
        s = case _callType of
          Asm.CallFun {} -> 0
          Asm.CallClosure -> 1

    mkCallClosures :: Bool -> Asm.InstrCallClosures -> Instruction
    mkCallClosures isTail Asm.InstrCallClosures {..}
      | not isTail =
          CallClosures $
            InstrCallClosures
              { _instrCallClosuresResult = mkVarRef VarGroupLocal (ntmps + m),
                _instrCallClosuresValue = mkVarRef VarGroupLocal (ntmps + n),
                _instrCallClosuresArgs = getArgs 1 _callClosuresArgsNum,
                _instrCallClosuresLiveVars = liveVars (_callClosuresArgsNum + 1)
              }
      | otherwise =
          TailCallClosures $
            InstrTailCallClosures
              { _instrTailCallClosuresValue = mkVarRef VarGroupLocal (ntmps + n),
                _instrTailCallClosuresArgs = getArgs 1 _callClosuresArgsNum
              }
      where
        -- note: the value (closure) is also on the stack
        m = n - _callClosuresArgsNum

fromAsmBranch ::
  Asm.FunctionInfo ->
  Asm.StackInfo ->
  Asm.CmdBranch ->
  Code ->
  Code ->
  Sem r Instruction
fromAsmBranch fi si Asm.CmdBranch {} codeTrue codeFalse =
  return $
    Branch $
      InstrBranch
        { _instrBranchValue = VRef $ mkVarRef VarGroupLocal (fromJust (fi ^. Asm.functionExtra) ^. Asm.functionMaxTempStackHeight + si ^. Asm.stackInfoValueStackHeight - 1),
          _instrBranchTrue = codeTrue,
          _instrBranchFalse = codeFalse
        }

fromAsmCase ::
  Asm.FunctionInfo ->
  Asm.InfoTable ->
  Asm.StackInfo ->
  Asm.CmdCase ->
  [Code] ->
  Maybe Code ->
  Sem r Instruction
fromAsmCase fi tab si Asm.CmdCase {..} brs def =
  return $
    Case $
      InstrCase
        { _instrCaseValue = VRef $ mkVarRef VarGroupLocal (fromJust (fi ^. Asm.functionExtra) ^. Asm.functionMaxTempStackHeight + si ^. Asm.stackInfoValueStackHeight - 1),
          _instrCaseInductive = _cmdCaseInductive,
          _instrCaseIndRep = ii ^. Asm.inductiveRepresentation,
          _instrCaseBranches =
            zipWithExact
              ( \br code ->
                  let tag = br ^. Asm.caseBranchTag
                      ci =
                        fromMaybe impossible $
                          HashMap.lookup tag (tab ^. Asm.infoConstrs)
                   in CaseBranch
                        { _caseBranchTag = tag,
                          _caseBranchMemRep = ci ^. Asm.constructorRepresentation,
                          _caseBranchArgsNum = ci ^. Asm.constructorArgsNum,
                          _caseBranchCode = code
                        }
              )
              _cmdCaseBranches
              brs,
          _instrCaseDefault = def
        }
  where
    ii =
      fromMaybe impossible $
        HashMap.lookup _cmdCaseInductive (tab ^. Asm.infoInductives)

fromAsmSave ::
  Asm.FunctionInfo ->
  Asm.StackInfo ->
  Asm.CmdSave ->
  Code ->
  Sem r Instruction
fromAsmSave fi si Asm.CmdSave {..} block =
  return $
    Block $
      InstrBlock
        { _instrBlockCode =
            Assign
              ( InstrAssign
                  (VarRef VarGroupLocal (si ^. Asm.stackInfoTempStackHeight) _cmdSaveName)
                  (VRef $ mkVarRef VarGroupLocal (fromJust (fi ^. Asm.functionExtra) ^. Asm.functionMaxTempStackHeight + si ^. Asm.stackInfoValueStackHeight - 1))
              )
              : block
        }
