module Juvix.Compiler.Asm.Extra.Base where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Language

mkInstr :: Instruction -> Command
mkInstr = Instr . CmdInstr emptyInfo

mkBinop :: Opcode -> Command
mkBinop = mkInstr . Binop

mkInstr' :: Maybe Location -> Instruction -> Command
mkInstr' loc = Instr . CmdInstr (CommandInfo loc)

mkBinop' :: Maybe Location -> Opcode -> Command
mkBinop' loc = mkInstr' loc . Binop

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
