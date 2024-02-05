module Juvix.Compiler.Reg.Data.InfoTableBuilder
  ( module Juvix.Compiler.Reg.Data.InfoTableBuilder,
    module Juvix.Compiler.Tree.Data.InfoTableBuilder.Base,
  )
where

import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Language
import Juvix.Compiler.Tree.Data.InfoTableBuilder.Base

type InfoTableBuilder = InfoTableBuilder' Code FunctionInfoExtra

type BuilderState = BuilderState' Code FunctionInfoExtra

freshSymbol :: (Member InfoTableBuilder r) => Sem r Symbol
freshSymbol = freshSymbol' @Code @FunctionInfoExtra

freshTag :: (Member InfoTableBuilder r) => Sem r Tag
freshTag = freshTag' @Code @FunctionInfoExtra

registerFunction :: (Member InfoTableBuilder r) => FunctionInfo -> Sem r ()
registerFunction = registerFunction' @Code @FunctionInfoExtra

registerConstr :: (Member InfoTableBuilder r) => ConstructorInfo -> Sem r ()
registerConstr = registerConstr' @Code @FunctionInfoExtra

registerInductive :: (Member InfoTableBuilder r) => InductiveInfo -> Sem r ()
registerInductive = registerInductive' @Code @FunctionInfoExtra

registerForward :: (Member InfoTableBuilder r) => Text -> Symbol -> Sem r ()
registerForward = registerForward' @Code @FunctionInfoExtra

registerMain :: (Member InfoTableBuilder r) => Symbol -> Sem r ()
registerMain = registerMain' @Code @FunctionInfoExtra

getIdent :: (Member InfoTableBuilder r) => Text -> Sem r (Maybe IdentKind)
getIdent = getIdent' @Code @FunctionInfoExtra

getFunctionInfo :: (Member InfoTableBuilder r) => Symbol -> Sem r FunctionInfo
getFunctionInfo = getFunctionInfo' @Code @FunctionInfoExtra

getConstructorInfo :: (Member InfoTableBuilder r) => Tag -> Sem r ConstructorInfo
getConstructorInfo = getConstructorInfo' @Code @FunctionInfoExtra
