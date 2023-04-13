module Juvix.Compiler.Core.Transformation.CheckExec where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Data.PPOutput

checkExec :: forall r. Member (Error CoreError) r => InfoTable -> Sem r InfoTable
checkExec tab =
  case tab ^. infoMain of
    Nothing -> return tab
    Just sym ->
      case ii ^. identifierType of
        NPi {} ->
          throw
            CoreError
              { _coreErrorMsg = ppOutput "`main` cannot have a function type for this target",
                _coreErrorNode = Nothing,
                _coreErrorLoc = loc
              }
        ty
          | isTypeConstr tab ty ->
              throw
                CoreError
                  { _coreErrorMsg = ppOutput "`main` cannot be a type for this target",
                    _coreErrorNode = Nothing,
                    _coreErrorLoc = loc
                  }
        _ ->
          return tab
      where
        ii = lookupIdentifierInfo tab sym
        loc = fromMaybe defaultLoc (ii ^. identifierLocation)

        mockFile :: Path Abs File
        mockFile = $(mkAbsFile "/core-to-exec")

        defaultLoc :: Interval
        defaultLoc = singletonInterval (mkInitialLoc mockFile)
