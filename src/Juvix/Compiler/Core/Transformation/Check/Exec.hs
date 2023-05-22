module Juvix.Compiler.Core.Transformation.Check.Exec where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Check.Base
import Juvix.Data.PPOutput

checkExec :: forall r. Member (Error CoreError) r => InfoTable -> Sem r InfoTable
checkExec tab =
  case tab ^. infoMain of
    Nothing ->
      throw
        CoreError
          { _coreErrorMsg = ppOutput "no `main` function",
            _coreErrorNode = Nothing,
            _coreErrorLoc = defaultLoc
          }
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
