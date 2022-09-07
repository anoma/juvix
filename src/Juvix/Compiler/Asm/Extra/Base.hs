module Juvix.Compiler.Asm.Extra.Base where

import Juvix.Compiler.Asm.Language

mkInstr :: Instruction -> Command
mkInstr = Instr . CmdInstr emptyInfo
