module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking.Data.Types where

import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Language

data InferenceEnv = InferenceEnv
  { -- | The context of the term being inferred.
    _inferenceEnvContext :: Context Object,
    -- | A Geb object to help the inference process.
    -- This is needed because some morphisms lack of type information.
    -- For example, the case of the left injection of a coproduct.
    _inferenceEnvTypeInfo :: Maybe Object
  }
  deriving stock (Show, Generic)

makeLenses ''InferenceEnv

defaultInferenceEnv :: InferenceEnv
defaultInferenceEnv =
  InferenceEnv
    { _inferenceEnvContext = Context.empty,
      _inferenceEnvTypeInfo = Nothing
    }
