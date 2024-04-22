module Juvix.Compiler.Asm.Extra.Base where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Language

mkInstr :: Instruction -> Command
mkInstr = Instr . CmdInstr emptyInfo

mkBinop :: BinaryOp -> Command
mkBinop = mkInstr . Binop

mkInstr' :: Maybe Location -> Instruction -> Command
mkInstr' loc = Instr . CmdInstr (CommandInfo loc)

mkBinop' :: Maybe Location -> BinaryOp -> Command
mkBinop' loc = mkInstr' loc . Binop

mkUnop :: UnaryOp -> Command
mkUnop = mkInstr . Unop

mkUnop' :: Maybe Location -> UnaryOp -> Command
mkUnop' loc = mkInstr' loc . Unop

mkCairo :: CairoOp -> Command
mkCairo = mkInstr . Cairo

mkCairo' :: Maybe Location -> CairoOp -> Command
mkCairo' loc = mkInstr' loc . Cairo

isFinalInstr :: Instruction -> Bool
isFinalInstr = \case
  Return -> True
  TailCall {} -> True
  TailCallClosures {} -> True
  Failure {} -> False
  _ -> False

getConstrSize :: MemRep -> Int -> Int
getConstrSize rep argsNum = case rep of
  MemRepConstr -> 1 + argsNum
  MemRepTag -> 0
  MemRepTuple -> argsNum
  MemRepUnit -> 0
  MemRepUnpacked {} -> 0

getCommandInfo :: Command -> CommandInfo
getCommandInfo = \case
  Instr CmdInstr {..} -> _cmdInstrInfo
  Branch CmdBranch {..} -> _cmdBranchInfo
  Case CmdCase {..} -> _cmdCaseInfo
  Save CmdSave {..} -> _cmdSaveInfo

getCommandLocation :: Command -> Maybe Location
getCommandLocation = (^. commandInfoLocation) . getCommandInfo
