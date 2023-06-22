module Juvix.Compiler.Core.Transformation.Check.VampIR where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Check.Base
import Juvix.Data.PPOutput

checkVampIR :: forall r. Member (Error CoreError) r => InfoTable -> Sem r InfoTable
checkVampIR tab =
  checkMainExists tab
    >> checkMainType
    >> checkPublicInputs
    >> checkNoAxioms tab
    >> mapAllNodesM checkNoIO tab
    >> mapAllNodesM (checkBuiltins True) tab
  where
    checkMainType :: Sem r ()
    checkMainType =
      unless (checkType (ii ^. identifierType)) $
        throw
          CoreError
            { _coreErrorMsg = ppOutput "for this target the arguments and the result of the `main` function must be numbers or booleans",
              _coreErrorLoc = fromMaybe defaultLoc (ii ^. identifierLocation),
              _coreErrorNode = Nothing
            }
      where
        ii = lookupIdentifierInfo tab (fromJust (tab ^. infoMain))

    checkType :: Node -> Bool
    checkType ty =
      let (tyargs, tgt) = unfoldPi' ty
       in all isPrimIntegerOrBool (tgt : tyargs)
      where
        isPrimIntegerOrBool ty' =
          isTypeInteger ty' || isTypeBool ty' || isDynamic ty'

    checkPublicInputs :: Sem r ()
    checkPublicInputs =
      unless (maybe True (all (`elem` argnames) . (^. pragmaPublic)) (ii ^. identifierPragmas . pragmasPublic)) $
        throw
          CoreError
            { _coreErrorMsg = ppOutput "invalid public input: not an argument name",
              _coreErrorLoc = fromMaybe defaultLoc (ii ^. identifierLocation),
              _coreErrorNode = Nothing
            }
      where
        ii = lookupIdentifierInfo tab (fromJust (tab ^. infoMain))
        argnames = map (fromMaybe "") (ii ^. identifierArgNames)
