module Juvix.Compiler.Core.Transformation.CheckGeb where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.LocationInfo (getInfoLocation)
import Juvix.Compiler.Core.Transformation.Base

checkGeb :: forall r. Member (Error CoreError) r => InfoTable -> Sem r InfoTable
checkGeb tab = checkNoRecursion >> mapAllNodesM checkTypes tab
  where
    checkTypes :: Node -> Sem r Node
    checkTypes = dmapM go
      where
        go :: Node -> Sem r Node
        go node = case node of
          NIdt Ident {..}
            | isDynamic (fromJust (HashMap.lookup _identSymbol (tab ^. infoIdentifiers)) ^. identifierType) ->
                throw (dynamicTypeError node (getInfoLocation _identInfo))
          NLam Lambda {..}
            | isDynamic (_lambdaBinder ^. binderType) ->
                throw (dynamicTypeError node (_lambdaBinder ^. binderLocation))
          NPi (Pi {..})
            | isTypeConstr tab (_piBinder ^. binderType) ->
                throw
                  CoreError
                    { _coreErrorMsg = "polymorphism not supported for the GEB target",
                      _coreErrorNode = Just node,
                      _coreErrorLoc = fromMaybe defaultLoc (getInfoLocation _piInfo)
                    }
          _ -> return node

        dynamicTypeError :: Node -> Maybe Location -> CoreError
        dynamicTypeError node loc =
          CoreError
            { _coreErrorMsg = "compilation for the GEB target requires full type information",
              _coreErrorNode = Just node,
              _coreErrorLoc = fromMaybe defaultLoc loc
            }

    checkNoRecursion :: Sem r ()
    checkNoRecursion =
      if
          | isCyclic (createIdentDependencyInfo tab) ->
              throw
                CoreError
                  { _coreErrorMsg = "recursion not supported for the GEB target",
                    _coreErrorNode = Nothing,
                    _coreErrorLoc = defaultLoc
                  }
          | otherwise ->
              return ()

    mockFile :: Path Abs File
    mockFile = $(mkAbsFile "/core-to-geb")

    defaultLoc :: Interval
    defaultLoc = singletonInterval (mkInitialLoc mockFile)
