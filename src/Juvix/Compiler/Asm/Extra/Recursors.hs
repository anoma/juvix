module Juvix.Compiler.Asm.Extra.Recursors
  ( module Juvix.Compiler.Asm.Extra.Recursors,
    Arguments,
  )
where

import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Extra.Memory
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Language.Type

data RecursorSig r a = RecursorSig
  { _recurseInstr :: Memory -> CmdInstr -> Sem r a,
    _recurseBranch :: Memory -> CmdBranch -> a -> a -> Sem r a,
    _recurseCase :: Memory -> CmdCase -> [a] -> Maybe a -> Sem r a
  }

makeLenses ''RecursorSig

recurse :: Member (Error AsmError) r => RecursorSig r a -> Arguments -> Code -> Sem r [a]
recurse sig args = fmap snd . recurse' sig (mkMemory args)

recurse' :: forall r a. Member (Error AsmError) r => RecursorSig r a -> Memory -> Code -> Sem r (Memory, [a])
recurse' sig = go
  where
    go :: Memory -> Code -> Sem r (Memory, [a])
    go mem = \case
      [] -> return (mem, [])
      h : t -> case h of
        Instr x -> goNextCmd (goInstr mem x) t
        Branch x -> goNextCmd (goBranch mem x) t
        Case x -> goNextCmd (goCase mem x) t

    goNextCmd :: Sem r (Memory, a) -> Code -> Sem r (Memory, [a])
    goNextCmd mp t = do
      (mem', r) <- mp
      (mem'', rs) <- go mem' t
      return (mem'', r : rs)

    goInstr :: Memory -> CmdInstr -> Sem r (Memory, a)
    goInstr mem cmd@CmdInstr {..} = case _cmdInstrInstruction of
      IntAdd -> goIntOp mem cmd
      IntSub -> goIntOp mem cmd
      IntMul -> goIntOp mem cmd
      IntDiv -> goIntOp mem cmd
      IntLt -> goBinOp mkInteger mkInteger mkBool mem cmd
      IntLe -> goBinOp mkInteger mkInteger mkBool mem cmd
      ValEq -> goBinOp TyDynamic TyDynamic mkBool mem cmd
      Push val -> do
        ty <- getValueType loc mem val
        a <- (sig ^. recurseInstr) mem cmd
        return (pushValueStack ty mem, a)
      Pop -> do
        when (null (mem ^. memoryValueStack)) $
          throw $ AsmError loc "popping empty value stack"
        a <- (sig ^. recurseInstr) mem cmd
        return (popValueStack 1 mem, a)
      PushTemp -> do
        when (null (mem ^. memoryValueStack)) $
          throw $ AsmError loc "popping empty value stack"
        a <- (sig ^. recurseInstr) mem cmd
        let mem' = pushTempStack (topValueStack' 0 mem) mem
        return (mem', a)
      PopTemp -> do
        when (null (mem ^. memoryTempStack)) $
          throw $ AsmError loc "popping empty temporary stack"
        a <- (sig ^. recurseInstr) mem cmd
        return (popTempStack 1 mem, a)
      AllocConstr tag -> undefined
      AllocClosure _ -> undefined
      ExtendClosure _ -> undefined
      Call _ -> undefined
      TailCall _ -> undefined
      CallClosures _ -> undefined
      TailCallClosures _ -> undefined
      Return -> undefined
      where
        loc = _cmdInstrInfo ^. commandInfoLocation

    goIntOp :: Memory -> CmdInstr -> Sem r (Memory, a)
    goIntOp = goBinOp mkInteger mkInteger mkInteger

    goBinOp :: Type -> Type -> Type -> Memory -> CmdInstr -> Sem r (Memory, a)
    goBinOp ty1 ty0 rty mem cmd@CmdInstr {..} = do
      checkValueStack loc [ty1, ty0] mem
      a <- (sig ^. recurseInstr) mem cmd
      let mem' = pushValueStack rty (popValueStack 2 mem)
      return (mem', a)
      where
        loc = _cmdInstrInfo ^. commandInfoLocation

    goBranch :: Memory -> CmdBranch -> Sem r (Memory, a)
    goBranch mem = undefined

    goCase :: Memory -> CmdCase -> Sem r (Memory, a)
    goCase mem = undefined

    getValueType :: Maybe Location -> Memory -> Value -> Sem r Type
    getValueType loc mem = \case
      ConstInt _ -> return mkInteger
      ConstBool _ -> return mkBool
      ConstString _ -> return TyString
      Ref val -> case getMemValueType val mem of
        Just ty -> return ty
        Nothing -> throw $ AsmError loc "invalid memory reference"
