module Juvix.Compiler.Asm.Data.InfoTableBuilder
  ( module Juvix.Compiler.Asm.Data.InfoTableBuilder,
    module Juvix.Compiler.Tree.Data.InfoTableBuilder.Base,
  )
where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Tree.Data.InfoTableBuilder.Base

type InfoTableBuilder = InfoTableBuilder' Code (Maybe FunctionInfoExtra)

type BuilderState = BuilderState' Code (Maybe FunctionInfoExtra)

freshSymbol :: (Member InfoTableBuilder r) => Sem r Symbol
freshSymbol = send @InfoTableBuilder FreshSymbol

freshTag :: (Member InfoTableBuilder r) => Sem r Tag
freshTag = send @InfoTableBuilder FreshTag

registerFunction :: (Member InfoTableBuilder r) => FunctionInfo -> Sem r ()
registerFunction fi = send @InfoTableBuilder (RegisterFunction fi)

registerConstr :: (Member InfoTableBuilder r) => ConstructorInfo -> Sem r ()
registerConstr ci = send @InfoTableBuilder (RegisterConstr ci)

registerInductive :: (Member InfoTableBuilder r) => InductiveInfo -> Sem r ()
registerInductive ii = send @InfoTableBuilder (RegisterInductive ii)

registerForward :: (Member InfoTableBuilder r) => Text -> Symbol -> Sem r ()
registerForward name sym = send @InfoTableBuilder (RegisterForward name sym)

registerMain :: (Member InfoTableBuilder r) => Symbol -> Sem r ()
registerMain sym = send @InfoTableBuilder (RegisterMain sym)

getIdent :: (Member InfoTableBuilder r) => Text -> Sem r (Maybe IdentKind)
getIdent name = send @InfoTableBuilder (GetIdent name)

getFunctionInfo :: (Member InfoTableBuilder r) => Symbol -> Sem r FunctionInfo
getFunctionInfo sym = send @InfoTableBuilder (GetFunctionInfo sym)
