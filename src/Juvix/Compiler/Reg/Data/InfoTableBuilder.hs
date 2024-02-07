module Juvix.Compiler.Reg.Data.InfoTableBuilder
  ( module Juvix.Compiler.Reg.Data.InfoTableBuilder,
    module Juvix.Compiler.Tree.Data.InfoTableBuilder.Base,
  )
where

import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Language
import Juvix.Compiler.Tree.Data.InfoTableBuilder.Base

type InfoTableBuilder = InfoTableBuilder' Code ()

type BuilderState = BuilderState' Code ()

freshSymbol :: (Member InfoTableBuilder r) => Sem r Symbol
freshSymbol = freshSymbol' @Code @()

freshTag :: (Member InfoTableBuilder r) => Sem r Tag
freshTag = freshTag' @Code @()

registerFunction :: (Member InfoTableBuilder r) => FunctionInfo -> Sem r ()
registerFunction = registerFunction' @Code @()

registerConstr :: (Member InfoTableBuilder r) => ConstructorInfo -> Sem r ()
registerConstr = registerConstr' @Code @()

registerInductive :: (Member InfoTableBuilder r) => InductiveInfo -> Sem r ()
registerInductive = registerInductive' @Code @()

registerForward :: (Member InfoTableBuilder r) => Text -> Symbol -> Sem r ()
registerForward = registerForward' @Code @()

registerMain :: (Member InfoTableBuilder r) => Symbol -> Sem r ()
registerMain = registerMain' @Code @()

getIdent :: (Member InfoTableBuilder r) => Text -> Sem r (Maybe IdentKind)
getIdent = getIdent' @Code @()

getFunctionInfo :: (Member InfoTableBuilder r) => Symbol -> Sem r FunctionInfo
getFunctionInfo = getFunctionInfo' @Code @()

getConstructorInfo :: (Member InfoTableBuilder r) => Tag -> Sem r ConstructorInfo
getConstructorInfo = getConstructorInfo' @Code @()
