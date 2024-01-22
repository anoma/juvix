module Juvix.Compiler.Tree.Translation.FromAsm where

import Juvix.Compiler.Asm.Data.InfoTable qualified as Asm
import Juvix.Compiler.Asm.Language qualified as Asm
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Language
import Juvix.Compiler.Tree.Translation.FromAsm.Translator

fromAsm :: (Member (Error TreeError) r) => Asm.InfoTable -> Sem r InfoTable
fromAsm tab = do
  fns <- mapM (goFunction tab) (tab ^. Asm.infoFunctions)
  return $
    InfoTable
      { _infoMainFunction = tab ^. Asm.infoMainFunction,
        _infoFunctions = fns,
        _infoInductives = tab ^. Asm.infoInductives,
        _infoConstrs = tab ^. Asm.infoConstrs
      }

goFunction :: (Member (Error TreeError) r') => Asm.InfoTable -> Asm.FunctionInfo -> Sem r' FunctionInfo
goFunction infoTab fi = do
  node' <- goCodeBlock (fi ^. Asm.functionCode)
  return $
    FunctionInfo
      { _functionName = fi ^. Asm.functionName,
        _functionLocation = fi ^. Asm.functionLocation,
        _functionSymbol = fi ^. Asm.functionSymbol,
        _functionArgsNum = fi ^. Asm.functionArgsNum,
        _functionArgNames = fi ^. Asm.functionArgNames,
        _functionType = fi ^. Asm.functionType,
        _functionCode = node',
        _functionExtra = ()
      }
  where
    unsupported :: (Member (Error TreeError) r) => Maybe Location -> Sem r a
    unsupported loc = throw $ TreeError loc "unsupported"

    goCodeBlock :: forall r. (Member (Error TreeError) r) => Asm.Code -> Sem r Node
    goCodeBlock cmds = runTranslator (reverse cmds) goCode

    goCode :: forall r. (Members '[Error TreeError, Translator] r) => Sem r Node
    goCode = do
      cmd <- nextCommand
      case cmd of
        Asm.Instr x -> goInstr x
        Asm.Branch x -> goBranch x
        Asm.Case x -> goCase x
        Asm.Save x -> goSave x
      where
        goInstr :: Asm.CmdInstr -> Sem r Node
        goInstr Asm.CmdInstr {..} = case _cmdInstrInstruction of
          Asm.Binop op -> goBinop (translateBinop op)
          Asm.ValShow -> goUnop OpShow
          Asm.StrToInt -> goUnop OpStrToInt
          Asm.Push (Asm.Constant c) -> return (Const c)
          Asm.Push (Asm.Ref r) -> return (MemRef r)
          Asm.Pop -> goBinop OpSeq
          Asm.Trace -> goUnop OpTrace
          Asm.Dump -> unsupported (_cmdInstrInfo ^. Asm.commandInfoLocation)
          Asm.Failure -> goUnop OpFail
          Asm.ArgsNum -> goUnop OpArgsNum
          Asm.Prealloc {} -> unsupported (_cmdInstrInfo ^. Asm.commandInfoLocation)
          Asm.AllocConstr tag -> goAllocConstr tag
          Asm.AllocClosure x -> goAllocClosure x
          Asm.ExtendClosure x -> goExtendClosure x
          Asm.Call x -> goCall x
          Asm.TailCall x -> goCall x
          Asm.CallClosures x -> goCallClosures x
          Asm.TailCallClosures x -> goCallClosures x
          Asm.Return -> goCode

        goBranch :: Asm.CmdBranch -> Sem r Node
        goBranch Asm.CmdBranch {..} = do
          arg <- goCode
          br1 <- goCodeBlock _cmdBranchTrue
          br2 <- goCodeBlock _cmdBranchFalse
          return $
            Branch
              NodeBranch
                { _nodeBranchArg = arg,
                  _nodeBranchTrue = br1,
                  _nodeBranchFalse = br2
                }

        goCase :: Asm.CmdCase -> Sem r Node
        goCase Asm.CmdCase {..} = do
          arg <- goCode
          brs <- mapM (goCaseBranch (_cmdCaseInfo ^. Asm.commandInfoLocation)) _cmdCaseBranches
          def <- maybe (return Nothing) (fmap Just . goCodeBlock) _cmdCaseDefault
          return $
            Case
              NodeCase
                { _nodeCaseInductive = _cmdCaseInductive,
                  _nodeCaseArg = arg,
                  _nodeCaseBranches = brs,
                  _nodeCaseDefault = def
                }

        goCaseBranch :: Maybe Location -> Asm.CaseBranch -> Sem r CaseBranch
        goCaseBranch loc Asm.CaseBranch {..} = case _caseBranchCode of
          [Asm.Save Asm.CmdSave {..}] -> do
            body <- goCodeBlock _cmdSaveCode
            return $
              CaseBranch
                { _caseBranchTag,
                  _caseBranchBody = body,
                  _caseBranchSave = True
                }
          Asm.Instr (Asm.CmdInstr _ Asm.Pop) : cmds -> do
            body <- goCodeBlock cmds
            return $
              CaseBranch
                { _caseBranchTag,
                  _caseBranchBody = body,
                  _caseBranchSave = False
                }
          _ ->
            throw
              TreeError
                { _treeErrorMsg = "expected 'save' or 'pop' at the beginning of case branch code",
                  _treeErrorLoc = loc
                }

        goSave :: Asm.CmdSave -> Sem r Node
        goSave Asm.CmdSave {..} = do
          arg <- goCode
          body <- goCodeBlock _cmdSaveCode
          return $
            Save
              NodeSave
                { _nodeSaveName = _cmdSaveName,
                  _nodeSaveArg = arg,
                  _nodeSaveBody = body
                }

        translateBinop :: Asm.Opcode -> BinaryOpcode
        translateBinop = \case
          Asm.IntAdd -> IntAdd
          Asm.IntSub -> IntSub
          Asm.IntMul -> IntMul
          Asm.IntDiv -> IntDiv
          Asm.IntMod -> IntMod
          Asm.IntLt -> IntLt
          Asm.IntLe -> IntLe
          Asm.ValEq -> ValEq
          Asm.StrConcat -> StrConcat

        goBinop :: BinaryOpcode -> Sem r Node
        goBinop op = do
          arg1 <- goCode
          arg2 <- goCode
          return $
            Binop
              NodeBinop
                { _nodeBinopOpcode = op,
                  _nodeBinopArg1 = arg1,
                  _nodeBinopArg2 = arg2
                }

        goUnop :: UnaryOpcode -> Sem r Node
        goUnop op = do
          arg <- goCode
          return $
            Unop
              NodeUnop
                { _nodeUnopArg = arg,
                  _nodeUnopOpcode = op
                }

        goArgs :: Int -> Sem r [Node]
        goArgs n = mapM (const goCode) [1 .. n]

        goAllocConstr :: Tag -> Sem r Node
        goAllocConstr tag = do
          args <- goArgs argsNum
          return $
            AllocConstr
              NodeAllocConstr
                { _nodeAllocConstrTag = tag,
                  _nodeAllocConstrArgs = args
                }
          where
            argsNum = Asm.lookupConstrInfo infoTab tag ^. constructorArgsNum

        goAllocClosure :: Asm.InstrAllocClosure -> Sem r Node
        goAllocClosure Asm.InstrAllocClosure {..} = do
          args <- goArgs _allocClosureArgsNum
          return $
            AllocClosure
              NodeAllocClosure
                { _nodeAllocClosureArgs = args,
                  _nodeAllocClosureFunSymbol = _allocClosureFunSymbol
                }

        goExtendClosure :: Asm.InstrExtendClosure -> Sem r Node
        goExtendClosure Asm.InstrExtendClosure {..} = do
          cl <- goCode
          args <- goArgs _extendClosureArgsNum
          return $
            ExtendClosure
              NodeExtendClosure
                { _nodeExtendClosureArgs = nonEmpty' args,
                  _nodeExtendClosureFun = cl
                }

        goCall :: Asm.InstrCall -> Sem r Node
        goCall Asm.InstrCall {..} = case _callType of
          Asm.CallFun sym -> do
            args <- goArgs _callArgsNum
            return $
              Call
                NodeCall
                  { _nodeCallType = CallFun sym,
                    _nodeCallArgs = args
                  }
          Asm.CallClosure -> do
            cl <- goCode
            args <- goArgs _callArgsNum
            return $
              Call
                NodeCall
                  { _nodeCallType = CallClosure cl,
                    _nodeCallArgs = args
                  }

        goCallClosures :: Asm.InstrCallClosures -> Sem r Node
        goCallClosures Asm.InstrCallClosures {..} = do
          cl <- goCode
          args <- goArgs _callClosuresArgsNum
          return $
            CallClosures
              NodeCallClosures
                { _nodeCallClosuresFun = cl,
                  _nodeCallClosuresArgs = args
                }
