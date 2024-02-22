module Juvix.Compiler.Backend.VampIR.Pretty.Keywords where

import Juvix.Compiler.Backend.VampIR.Language
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str

kwDef :: Doc Ann
kwDef = keyword Str.vampirDef

kwEq :: Doc Ann
kwEq = keyword Str.vampirEq

kwNumber :: Doc Ann
kwNumber = keyword Str.vampirNumber

kwFail :: Doc Ann
kwFail = keyword Str.vampirFail

kwAdd :: Doc Ann
kwAdd = keyword Str.vampirAdd

kwSub :: Doc Ann
kwSub = keyword Str.vampirSub

kwMul :: Doc Ann
kwMul = keyword Str.vampirMul

kwDiv :: Doc Ann
kwDiv = keyword Str.vampirDiv

kwMod :: Doc Ann
kwMod = keyword Str.vampirMod

kwFieldAdd :: Doc Ann
kwFieldAdd = keyword Str.fadd

kwFieldSub :: Doc Ann
kwFieldSub = keyword Str.fsub

kwFieldMul :: Doc Ann
kwFieldMul = keyword Str.fmul

kwFieldDiv :: Doc Ann
kwFieldDiv = keyword Str.fdiv

kwEqual :: Doc Ann
kwEqual = keyword Str.vampirEqual

kwLessThan :: Doc Ann
kwLessThan = keyword Str.vampirLessThan

kwLessOrEqual :: Doc Ann
kwLessOrEqual = keyword Str.vampirLessOrEqual

kwIf :: Doc Ann
kwIf = keyword Str.vampirIf
