module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.Inference where

import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Prelude

-- TODO: infer and check,
-- Atm, only reconstructing the type of the term
inferObject :: Geb.Morphism -> Either JuvixError Geb.Object
inferObject = \case
  Geb.MorphismUnit -> Right Geb.ObjectTerminal
  Geb.MorphismInteger {} -> Right Geb.ObjectInteger
  Geb.MorphismLeft {} -> Left (error @JuvixError "Not enough information to infer the right type of the coproduct")
  Geb.MorphismRight {} -> Left (error @JuvixError "Not enough information to infer the right type of the coproduct")
  Geb.MorphismAbsurd x -> inferObject x
  Geb.MorphismPair pair ->
    Right $
      Geb.ObjectProduct $
        Geb.Product
          { _productLeft = pair ^. Geb.pairLeftType,
            _productRight = pair ^. Geb.pairRightType
          }
  Geb.MorphismCase c ->
    Right $
      Geb.ObjectHom $
        Geb.Hom
          { _homDomain =
              Geb.ObjectCoproduct $
                Geb.Coproduct
                  { _coproductLeft = c ^. Geb.caseLeftType,
                    _coproductRight = c ^. Geb.caseRightType
                  },
            _homCodomain = c ^. Geb.caseCodomainType
          }
  Geb.MorphismFirst p -> Right $ p ^. Geb.firstLeftType
  Geb.MorphismSecond p -> Right $ p ^. Geb.secondRightType
  Geb.MorphismLambda l ->
    Right $
      Geb.ObjectHom $
        Geb.Hom
          { _homDomain = l ^. Geb.lambdaVarType,
            _homCodomain = l ^. Geb.lambdaBodyType
          }
  Geb.MorphismApplication app ->
    Right $
      Geb.ObjectHom $
        Geb.Hom
          { _homDomain = app ^. Geb.applicationDomainType,
            _homCodomain = app ^. Geb.applicationCodomainType
          }
  --   Geb.ObjectProduct  $ Geb.Product
  --     <$>
  --         inferObject (pair ^. Geb.pairLeft)
  --     <*> inferObject (pair ^. Geb.pairRight)
  _ -> Left (error @JuvixError "TODO: inferObject")
