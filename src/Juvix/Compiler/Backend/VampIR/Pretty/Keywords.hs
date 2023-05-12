module Juvix.Compiler.Backend.VampIR.Pretty.Keywords where

import Juvix.Compiler.Backend.VampIR.Language
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str

kwDef :: Doc Ann
kwDef = keyword Str.vampirDef

kwEq :: Doc Ann
kwEq = keyword Str.vampirEq

kwAdd :: Doc Ann
kwAdd = keyword Str.vampirAdd

kwSub :: Doc Ann
kwSub = keyword Str.vampirSub

kwMul :: Doc Ann
kwMul = keyword Str.vampirMul

kwDiv :: Doc Ann
kwDiv = keyword Str.vampirDiv

kwEqual :: Doc Ann
kwEqual = keyword Str.vampirEqual

kwLessThan :: Doc Ann
kwLessThan = keyword Str.vampirLessThan

kwAnd :: Doc Ann
kwAnd = keyword Str.vampirAnd

kwOr :: Doc Ann
kwOr = keyword Str.vampirOr

kwIf :: Doc Ann
kwIf = keyword Str.vampirIf
