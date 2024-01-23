module Juvix.Compiler.Tree.Data.InfoTableBuilder
  ( module Juvix.Compiler.Tree.Data.InfoTableBuilder,
    module Juvix.Compiler.Tree.Data.InfoTableBuilder.Base,
  )
where

import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Data.InfoTableBuilder.Base
import Juvix.Compiler.Tree.Language

type InfoTableBuilder = InfoTableBuilder' Node ()

type BuilderState = BuilderState' Node ()

freshSymbol :: (Member InfoTableBuilder r) => Sem r Symbol
freshSymbol = freshSymbol' @Node @()

freshTag :: (Member InfoTableBuilder r) => Sem r Tag
freshTag = freshTag' @Node @()

registerFunction :: (Member InfoTableBuilder r) => FunctionInfo -> Sem r ()
registerFunction = registerFunction' @Node @()

registerConstr :: (Member InfoTableBuilder r) => ConstructorInfo -> Sem r ()
registerConstr = registerConstr' @Node @()

registerInductive :: (Member InfoTableBuilder r) => InductiveInfo -> Sem r ()
registerInductive = registerInductive' @Node @()

registerForward :: (Member InfoTableBuilder r) => Text -> Symbol -> Sem r ()
registerForward = registerForward' @Node @()

registerMain :: (Member InfoTableBuilder r) => Symbol -> Sem r ()
registerMain = registerMain' @Node @()

getIdent :: (Member InfoTableBuilder r) => Text -> Sem r (Maybe IdentKind)
getIdent = getIdent' @Node @()

getFunctionInfo :: (Member InfoTableBuilder r) => Symbol -> Sem r FunctionInfo
getFunctionInfo = getFunctionInfo' @Node @()
