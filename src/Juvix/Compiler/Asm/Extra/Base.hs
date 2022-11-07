module Juvix.Compiler.Asm.Extra.Base where

import Data.HashMap.Strict qualified as HashMap
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

getFunInfo :: InfoTable -> Symbol -> FunctionInfo
getFunInfo infoTable sym = fromMaybe (error "invalid function symbol") (HashMap.lookup sym (infoTable ^. infoFunctions))

getConstrInfo :: InfoTable -> Tag -> ConstructorInfo
getConstrInfo infoTable tag = fromMaybe (error "invalid constructor tag") (HashMap.lookup tag (infoTable ^. infoConstrs))

getInductiveInfo :: InfoTable -> Symbol -> InductiveInfo
getInductiveInfo infoTable sym = fromMaybe (error "invalid inductive symbol") (HashMap.lookup sym (infoTable ^. infoInductives))

isFinalInstr :: Instruction -> Bool
isFinalInstr = \case
  Return -> True
  TailCall {} -> True
  TailCallClosures {} -> True
  Failure -> True
  _ -> False

getConstrSize :: MemRep -> Int -> Int
getConstrSize rep argsNum = case rep of
  MemRepConstr -> 1 + argsNum
  MemRepTag -> 0
  MemRepTuple -> argsNum
  MemRepUnit -> 0
  MemRepUnpacked {} -> 0
