module Juvix.Compiler.Core.Transformation.Check.Cairo where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Check.Base
import Juvix.Data.PPOutput

checkCairo :: forall r. (Member (Error CoreError) r) => Module -> Sem r Module
checkCairo md = do
  checkMainExists md
  checkMainType
  checkNoAxioms md
  mapAllNodesM checkNoIO md
  mapAllNodesM (checkBuiltins' [OpStrConcat, OpStrToInt, OpShow] [PrimString]) md
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
        ii = lookupIdentifierInfo md (fromJust (getInfoMain md))

    checkType :: Node -> Bool
    checkType ty =
      let (tyargs, tgt) = unfoldPi' ty
       in all isPrimInteger (tgt : tyargs)
      where
        isPrimInteger ty' =
          isTypeInteger ty' || isDynamic ty'
