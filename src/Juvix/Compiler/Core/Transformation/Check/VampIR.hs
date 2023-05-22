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
    >> mapAllNodesM checkNoIO tab
    >> mapAllNodesM (checkBuiltins True) tab
  where
    checkMainType :: Sem r ()
    checkMainType =
      unless (checkType (ii ^. identifierType)) $
        throw
          CoreError
            { _coreErrorMsg = ppOutput "for this target the arguments and the result of the `main` function must be numbers",
              _coreErrorLoc = fromMaybe defaultLoc (ii ^. identifierLocation),
              _coreErrorNode = Nothing
            }
      where
        ii = lookupIdentifierInfo tab (fromJust (tab ^. infoMain))

    checkType :: Node -> Bool
    checkType ty =
      let (tyargs, tgt) = unfoldPi' ty
       in all isPrimInteger (tgt : tyargs)
      where
        isPrimInteger ty' = case ty' of
          NPrim (TypePrim _ (PrimInteger _)) -> True
          NDyn _ -> True
          _ -> False
