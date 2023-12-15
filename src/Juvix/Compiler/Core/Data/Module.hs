module Juvix.Compiler.Core.Data.Module
  ( module Juvix.Compiler.Core.Data.Module,
    module Juvix.Compiler.Core.Data.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Language

data Module = Module
  { _moduleId :: ModuleId,
    _moduleInfoTable :: InfoTable,
    -- | The imports table contains all dependencies, transitively. E.g., if the
    -- module M imports A but not B, but A imports B, then all identifiers from
    -- B will be in the imports table of M nonetheless.
    _moduleImportsTable :: InfoTable
  }

makeLenses ''Module

withInfoTable :: (Module -> Module) -> InfoTable -> InfoTable
withInfoTable f tab =
  f (moduleFromInfoTable tab) ^. moduleInfoTable

emptyModule :: Module
emptyModule = Module defaultModuleId mempty mempty

moduleFromInfoTable :: InfoTable -> Module
moduleFromInfoTable tab = Module defaultModuleId tab mempty

computeCombinedIdentContext :: Module -> IdentContext
computeCombinedIdentContext Module {..} =
  _moduleInfoTable ^. identContext <> _moduleImportsTable ^. identContext

computeCombinedInfoTable :: Module -> InfoTable
computeCombinedInfoTable Module {..} = _moduleInfoTable <> _moduleImportsTable

lookupInductiveInfo' :: Module -> Symbol -> Maybe InductiveInfo
lookupInductiveInfo' Module {..} sym =
  lookupTabInductiveInfo' _moduleInfoTable sym
    <|> lookupTabInductiveInfo' _moduleImportsTable sym

lookupConstructorInfo' :: Module -> Tag -> Maybe ConstructorInfo
lookupConstructorInfo' Module {..} tag =
  lookupTabConstructorInfo' _moduleInfoTable tag
    <|> lookupTabConstructorInfo' _moduleImportsTable tag

lookupIdentifierInfo' :: Module -> Symbol -> Maybe IdentifierInfo
lookupIdentifierInfo' Module {..} sym =
  lookupTabIdentifierInfo' _moduleInfoTable sym
    <|> lookupTabIdentifierInfo' _moduleImportsTable sym

lookupIdentifierNode' :: Module -> Symbol -> Maybe Node
lookupIdentifierNode' Module {..} sym =
  lookupTabIdentifierNode' _moduleInfoTable sym
    <|> lookupTabIdentifierNode' _moduleImportsTable sym

lookupSpecialisationInfo :: Module -> Symbol -> [SpecialisationInfo]
lookupSpecialisationInfo Module {..} sym =
  fromMaybe [] $
    lookupTabSpecialisationInfo' _moduleInfoTable sym
      <|> lookupTabSpecialisationInfo' _moduleImportsTable sym

lookupInductiveInfo :: Module -> Symbol -> InductiveInfo
lookupInductiveInfo m sym = fromJust $ lookupInductiveInfo' m sym

lookupConstructorInfo :: Module -> Tag -> ConstructorInfo
lookupConstructorInfo m tag = fromJust $ lookupConstructorInfo' m tag

lookupIdentifierInfo :: Module -> Symbol -> IdentifierInfo
lookupIdentifierInfo m sym = fromJust $ lookupIdentifierInfo' m sym

lookupIdentifierNode :: Module -> Symbol -> Node
lookupIdentifierNode m sym = fromJust $ lookupIdentifierNode' m sym

lookupBuiltinInductive :: Module -> BuiltinInductive -> Maybe InductiveInfo
lookupBuiltinInductive Module {..} b =
  lookupTabBuiltinInductive _moduleInfoTable b
    <|> lookupTabBuiltinInductive _moduleImportsTable b

lookupBuiltinConstructor :: Module -> BuiltinConstructor -> Maybe ConstructorInfo
lookupBuiltinConstructor Module {..} b =
  lookupTabBuiltinConstructor _moduleInfoTable b
    <|> lookupTabBuiltinConstructor _moduleImportsTable b

getInfoLiteralIntToNat :: Module -> Maybe Symbol
getInfoLiteralIntToNat Module {..} =
  _moduleInfoTable ^. infoLiteralIntToNat
    <|> _moduleImportsTable ^. infoLiteralIntToNat

getInfoLiteralIntToInt :: Module -> Maybe Symbol
getInfoLiteralIntToInt Module {..} =
  _moduleInfoTable ^. infoLiteralIntToInt
    <|> _moduleImportsTable ^. infoLiteralIntToInt

getInfoMain :: Module -> Maybe Symbol
getInfoMain Module {..} =
  _moduleInfoTable ^. infoMain
    <|> _moduleImportsTable ^. infoMain

identName :: Module -> Symbol -> Text
identName m = identName' (computeCombinedInfoTable m)

typeName :: Module -> Symbol -> Text
typeName m = typeName' (computeCombinedInfoTable m)

identNames :: Module -> HashSet Text
identNames m = identNames' (computeCombinedInfoTable m)

freshIdentName :: Module -> Text -> Text
freshIdentName m = freshName (identNames m)

pruneInfoTable :: Module -> Module
pruneInfoTable = over moduleInfoTable pruneInfoTable'
