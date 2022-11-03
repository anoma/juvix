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
          _functionStackVarsNum = fi ^. Asm.functionMaxValueStackHeight,
          _functionTempVarsNum = fi ^. Asm.functionMaxTempStackHeight,
          _functionCode = fromAsmFun tab fi
        }

    convertConstr :: Asm.ConstructorInfo -> ConstructorInfo
    convertConstr ci =
      ConstructorInfo
        { _constructorName = ci ^. Asm.constructorName,
          _constructorLocation = ci ^. Asm.constructorLocation,
          _constructorTag = ci ^. Asm.constructorTag,
          _constructorArgsNum = ci ^. Asm.constructorArgsNum,
          _constructorInductive = ci ^. Asm.constructorInductive,
          _constructorRepresentation = ci ^. Asm.constructorRepresentation
        }

    convertInductive :: Asm.InductiveInfo -> InductiveInfo
    convertInductive ii =
      InductiveInfo
        { _inductiveName = ii ^. Asm.inductiveName,
          _inductiveLocation = ii ^. Asm.inductiveLocation,
          _inductiveSymbol = ii ^. Asm.inductiveSymbol,
          _inductiveConstructors = map convertConstr (ii ^. Asm.inductiveConstructors),
          _inductiveRepresentation = ii ^. Asm.inductiveRepresentation
        }

fromAsmFun ::
  Asm.InfoTable ->
  Asm.FunctionInfo ->
  Code
fromAsmFun tab fi =
  case run $ runError $ Asm.recurseFun sig fi of
    Left err -> error (show err)
    Right code -> code
  where
    sig :: Asm.RecursorSig Asm.Memory (Error Asm.AsmError ': r) Instruction
    sig =
      Asm.RecursorSig
        { _recursorInfoTable = tab,
          _recurseInstr = fromAsmInstr tab,
          _recurseBranch = fromAsmBranch,
          _recurseCase = fromAsmCase tab
        }

fromAsmInstr ::
  Asm.InfoTable ->
  Asm.Memory ->
  Asm.CmdInstr ->
  Sem r Instruction
fromAsmInstr tab mem Asm.CmdInstr {..} =
  case _cmdInstrInstruction of
    Asm.Binop op -> return $ mkBinop (mkOpcode op)
    Asm.Push val -> return $ mkAssign (VarRef VarGroupStack (n + 1)) (mkValue val)
    Asm.Pop -> return Nop
    Asm.PushTemp ->
      return $
        mkAssign
          (VarRef VarGroupTemp (Asm.tempStackHeight mem))
          (VRef $ VarRef VarGroupStack n)
    Asm.PopTemp -> return Nop
    Asm.Trace -> return $ Trace $ InstrTrace (VRef $ VarRef VarGroupStack n)
    Asm.Dump -> return Dump
    Asm.Failure -> return $ Failure $ InstrFailure (VRef $ VarRef VarGroupStack n)
    Asm.AllocConstr tag -> return $ mkAlloc tag
    Asm.AllocClosure x -> return $ mkAllocClosure x
    Asm.ExtendClosure x -> return $ mkExtendClosure x
    Asm.Call x -> return $ mkCall False x
    Asm.TailCall x -> return $ mkCall True x
    Asm.CallClosures x -> return $ mkCallClosures False x
    Asm.TailCallClosures x -> return $ mkCallClosures True x
    Asm.Return -> return Return
  where
    -- `n` is the index of the top of the value stack *before* executing the
    -- instruction
    n :: Int
    n = Asm.valueStackHeight mem - 1

    -- Live variables *after* executing the instruction. TODO: proper liveness
    -- analysis in JuvixAsm.
    liveVars :: Int -> [VarRef]
    liveVars k =
      map (VarRef VarGroupStack) [0 .. n - k]
        ++ map (VarRef VarGroupTemp) [0 .. Asm.tempStackHeight mem]
        ++ map (VarRef VarGroupArgs) [0 .. (mem ^. Asm.memoryArgsNum)]

    getArgs :: Int -> Int -> [Value]
    getArgs s k = map (\i -> VRef $ VarRef VarGroupStack (n - i)) [s .. (s + k - 1)]

    mkBinop :: Opcode -> Instruction
    mkBinop op =
      Binop
        ( BinaryOp
            { _binaryOpCode = op,
              _binaryOpResult = VarRef VarGroupStack n,
              _binaryOpArg1 = VRef $ VarRef VarGroupStack n,
              _binaryOpArg2 = VRef $ VarRef VarGroupStack (n - 1)
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

    mkAssign :: VarRef -> Value -> Instruction
    mkAssign tgt src = Assign (InstrAssign tgt src)

    mkValue :: Asm.Value -> Value
    mkValue = \case
      Asm.ConstInt v -> ConstInt v
      Asm.ConstBool v -> ConstBool v
      Asm.ConstString v -> ConstString v
      Asm.ConstUnit -> ConstUnit
      Asm.ConstVoid -> ConstVoid
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
            ci = fromJust impossible $ HashMap.lookup _fieldTag (tab ^. Asm.infoConstrs)

    mkVar :: Asm.DirectRef -> VarRef
    mkVar = \case
      Asm.StackRef -> VarRef VarGroupStack n
      Asm.ArgRef idx -> VarRef VarGroupArgs idx
      Asm.TempRef idx -> VarRef VarGroupTemp idx

    mkAlloc :: Tag -> Instruction
    mkAlloc tag =
      Alloc $
        InstrAlloc
          { _instrAllocTag = tag,
            _instrAllocResult = VarRef VarGroupStack n,
            _instrAllocArgs = getArgs 0 (ci ^. Asm.constructorArgsNum),
            _instrAllocMemRep = ci ^. Asm.constructorRepresentation,
            _instrAllocLiveVars = liveVars (ci ^. Asm.constructorArgsNum)
          }
      where
        ci = fromJust impossible $ HashMap.lookup tag (tab ^. Asm.infoConstrs)

    mkAllocClosure :: Asm.InstrAllocClosure -> Instruction
    mkAllocClosure Asm.InstrAllocClosure {..} =
      AllocClosure $
        InstrAllocClosure
          { _instrAllocClosureSymbol = fi ^. Asm.functionSymbol,
            _instrAllocClosureResult = VarRef VarGroupStack n,
            _instrAllocClosureExpectedArgsNum = fi ^. Asm.functionArgsNum,
            _instrAllocClosureArgs = getArgs 0 _allocClosureArgsNum,
            _instrAllocClosureLiveVars = liveVars _allocClosureArgsNum
          }
      where
        fi = fromJust impossible $ HashMap.lookup _allocClosureFunSymbol (tab ^. Asm.infoFunctions)

    mkExtendClosure :: Asm.InstrExtendClosure -> Instruction
    mkExtendClosure Asm.InstrExtendClosure {..} =
      ExtendClosure $
        InstrExtendClosure
          { _instrExtendClosureResult = VarRef VarGroupStack n,
            _instrExtendClosureValue = VarRef VarGroupStack n,
            _instrExtendClosureArgs = getArgs 1 _extendClosureArgsNum,
            _instrExtendClosureLiveVars = liveVars (_extendClosureArgsNum + 1)
          }

    mkCall :: Bool -> Asm.InstrCall -> Instruction
    mkCall isTail Asm.InstrCall {..} =
      Call $
        InstrCall
          { _instrCallResult = VarRef VarGroupStack n,
            _instrCallType = ct,
            _instrCallIsTail = isTail,
            _instrCallArgs = getArgs s _callArgsNum,
            _instrCallLiveVars = liveVars (_callArgsNum + s)
          }
      where
        ct = case _callType of
          Asm.CallFun f -> CallFun f
          Asm.CallClosure -> CallClosure (VarRef VarGroupStack n)
        s = case _callType of
          Asm.CallFun {} -> 0
          Asm.CallClosure -> 1

    mkCallClosures :: Bool -> Asm.InstrCallClosures -> Instruction
    mkCallClosures isTail Asm.InstrCallClosures {..} =
      CallClosures $
        InstrCallClosures
          { _instrCallClosuresResult = VarRef VarGroupStack n,
            _instrCallClosuresValue = VarRef VarGroupStack n,
            _instrCallClosuresIsTail = isTail,
            _instrCallClosuresArgs = getArgs 1 _callClosuresArgsNum,
            _instrCallClosuresLiveVars = liveVars _callClosuresArgsNum
          }

fromAsmBranch ::
  Asm.Memory ->
  Asm.CmdBranch ->
  Code ->
  Code ->
  Sem r Instruction
fromAsmBranch mem Asm.CmdBranch {} codeTrue codeFalse =
  return $
    Branch $
      InstrBranch
        { _instrBranchValue = VRef $ VarRef VarGroupStack (Asm.valueStackHeight mem - 1),
          _instrBranchTrue = codeTrue,
          _instrBranchFalse = codeFalse
        }

fromAsmCase ::
  Asm.InfoTable ->
  Asm.Memory ->
  Asm.CmdCase ->
  [Code] ->
  Maybe Code ->
  Sem r Instruction
fromAsmCase tab mem Asm.CmdCase {..} brs def =
  return $
    Case $
      InstrCase
        { _instrCaseValue = VRef $ VarRef VarGroupStack (Asm.valueStackHeight mem - 1),
          _instrCaseInductive = _cmdCaseInductive,
          _instrCaseIndRep = ii ^. inductiveRepresentation,
          _instrCaseBranches =
            zipWithExact
              ( \br code ->
                  let tag = br ^. Asm.caseBranchTag
                      ci =
                        fromJust impossible $
                          HashMap.lookup tag (tab ^. Asm.infoConstrs)
                   in CaseBranch
                        { _caseBranchTag = tag,
                          _caseBranchMemRep = ci ^. constructorRepresentation,
                          _caseBranchCode = code
                        }
              )
              _cmdCaseBranches
              brs,
          _instrCaseDefault = def
        }
  where
    ii =
      fromJust impossible $
        HashMap.lookup _cmdCaseInductive (tab ^. Asm.infoInductives)
