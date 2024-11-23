module Juvix.Compiler.Backend.Lean.Translation.FromTyped where

--import Data.HashMap.Strict qualified as HashMap
--import Data.HashSet qualified as HashSet
--import Data.List.NonEmpty.Extra qualified as NonEmpty
--import Data.Text qualified as T
--import Data.Text qualified as Text
import Juvix.Compiler.Backend.Lean.Data.Result
import Juvix.Compiler.Backend.Lean.Language
import Juvix.Compiler.Internal.Data.InfoTable qualified as Internal
import Juvix.Compiler.Internal.Extra qualified as Internal
--import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Extra
import Juvix.Compiler.Store.Language

fromInternal ::
  forall r.
  (Members '[Error JuvixError, Reader EntryPoint, Reader ModuleTable, NameIdGen] r) =>
  Internal.InternalTypedResult ->
  Sem r Result
fromInternal res@Internal.InternalTypedResult {..} = do
  itab <- getInternalModuleTable <$> ask
  let md :: Internal.InternalModule
      md = _resultInternalModule
      itab' :: Internal.InternalModuleTable
      itab' = Internal.insertInternalModule itab md
      table :: Internal.InfoTable
      table = Internal.computeCombinedInfoTable itab'
      comments :: [Comment]
      comments = allComments (Internal.getInternalTypedResultComments res)
  go comments table _resultModule
  where
    go :: [Comment] -> Internal.InfoTable -> Internal.Module -> Sem r Result
    go comments tab md =
      return $
        Result
          { _resultModule = goModule tab md,
            _resultModuleId = md ^. Internal.moduleId,
            _resultComments = filter (\c -> c ^. commentInterval . intervalFile == file) comments
          }
      where
        file = getLoc md ^. intervalFile

goModule :: Internal.InfoTable -> Internal.Module -> Module
goModule infoTable Internal.Module {..} =
  Module
    { _moduleName = _moduleName,
      _moduleImports = map (^. Internal.importModuleName) (_moduleBody ^. Internal.moduleImports),
      _moduleDeclarations = []
    }
