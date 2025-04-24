module Juvix.Compiler.Nockma.Data.Module
  ( module Juvix.Compiler.Nockma.Data.Module,
    module Juvix.Compiler.Nockma.Data.InfoTable,
    module Juvix.Compiler.Core.Data.Module.Base,
  )
where

import Juvix.Compiler.Core.Data.Module.Base
import Juvix.Compiler.Nockma.Data.InfoTable
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude

type Module = Module' InfoTable

type ModuleTable = ModuleTable' InfoTable

lookupFunInfo' :: Module -> Symbol -> Maybe FunctionInfo
lookupFunInfo' Module {..} sym =
  lookupTabFunInfo' _moduleInfoTable sym
    <|> lookupTabFunInfo' _moduleImportsTable sym

lookupFunInfo :: Module -> Symbol -> FunctionInfo
lookupFunInfo m sym = fromJust (lookupFunInfo' m sym)

getModuleCode :: Module -> Term Natural
getModuleCode Module {..} =
  fromJust (_moduleInfoTable ^. infoCode)

getModuleJammedCode :: Module -> ByteString
getModuleJammedCode Module {..} =
  fromJust (_moduleInfoTable ^. infoJammedCode)
