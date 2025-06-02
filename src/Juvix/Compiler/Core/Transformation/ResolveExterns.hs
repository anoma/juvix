module Juvix.Compiler.Core.Transformation.ResolveExterns
  ( resolveExterns,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Evaluator
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.FFI (FFI (..))
import Juvix.Compiler.FFI qualified as FFI

resolveExterns :: forall r. (Members '[Reader CoreOptions, Error CoreError] r) => Module -> Sem r Module
resolveExterns md = do
  opts <- ask @CoreOptions
  let combinedTab = computeCombinedInfoTable md
      externSyms = md ^. moduleInfoTable . infoExterns
      externInfos =
        map
          ( over identifierType (evalPartial (opts ^. optFieldSize) combinedTab)
              . lookupIdentifierInfo md
          )
          externSyms
      externNodes =
        map
          ( evalPartial (opts ^. optFieldSize) combinedTab
              . lookupIdentifierNode md
          )
          externSyms
  ffis <- foldl' (\acc (k, v) -> HashMap.insertWith (++) k [v] acc) mempty <$> mapM resolveFFI (zipExact externInfos externNodes)
  let identInfos = fmap (updateFFI ffis) (md ^. moduleInfoTable . infoIdentifiers)
  return $
    over
      moduleInfoTable
      ( set infoExterns []
          . set infoIdentifiers identInfos
      )
      md
  where
    resolveFFI :: (IdentifierInfo, Node) -> Sem r (Symbol, FFI)
    resolveFFI (info, node) =
      case info ^. identifierType of
        NTyp TypeConstr {..}
          | ii <- lookupInductiveInfo md _typeConstrSymbol ->
              case ii ^. inductiveBuiltin of
                Just (BuiltinTypeInductive BuiltinAnomaFFI) ->
                  maybe throwFFIError (return . second FFIAnoma) $
                    FFI.resolveAnomaFFI node
                _ ->
                  throwFFIError
        _ ->
          throwFFIError
      where
        throwFFIError :: Sem r a
        throwFFIError =
          throwError $
            CoreError
              { _coreErrorMsg = "Invalid FFI declaration",
                _coreErrorNode = Just node,
                _coreErrorLoc = fromMaybe defaultLoc $ info ^. identifierLocation
              }

    updateFFI :: HashMap Symbol [FFI] -> IdentifierInfo -> IdentifierInfo
    updateFFI ffis info = case HashMap.lookup (info ^. identifierSymbol) ffis of
      Just ffiList -> over identifierFFI (ffiList ++) info
      Nothing -> info

    defaultLoc :: Interval
    defaultLoc = singletonInterval (mkInitialLoc mockFile)
      where
        mockFile :: Path Abs File
        mockFile = $(mkAbsFile "/resolve-externs")
