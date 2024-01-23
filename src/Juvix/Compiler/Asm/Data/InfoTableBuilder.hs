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
freshSymbol = freshSymbol' @Code @(Maybe FunctionInfoExtra)

freshTag :: (Member InfoTableBuilder r) => Sem r Tag
freshTag = freshTag' @Code @(Maybe FunctionInfoExtra)

registerFunction :: (Member InfoTableBuilder r) => FunctionInfo -> Sem r ()
registerFunction = registerFunction' @Code @(Maybe FunctionInfoExtra)

registerConstr :: (Member InfoTableBuilder r) => ConstructorInfo -> Sem r ()
registerConstr = registerConstr' @Code @(Maybe FunctionInfoExtra)

registerInductive :: (Member InfoTableBuilder r) => InductiveInfo -> Sem r ()
registerInductive = registerInductive' @Code @(Maybe FunctionInfoExtra)

registerForward :: (Member InfoTableBuilder r) => Text -> Symbol -> Sem r ()
registerForward = registerForward' @Code @(Maybe FunctionInfoExtra)

registerMain :: (Member InfoTableBuilder r) => Symbol -> Sem r ()
registerMain = registerMain' @Code @(Maybe FunctionInfoExtra)

getIdent :: (Member InfoTableBuilder r) => Text -> Sem r (Maybe IdentKind)
getIdent = getIdent' @Code @(Maybe FunctionInfoExtra)

getFunctionInfo :: (Member InfoTableBuilder r) => Symbol -> Sem r FunctionInfo
getFunctionInfo = getFunctionInfo' @Code @(Maybe FunctionInfoExtra)
