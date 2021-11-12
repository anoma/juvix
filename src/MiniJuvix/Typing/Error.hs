module MiniJuvix.Typing.Error
  ( Error (..),
    CheckingError (..),
    CommonError (..),
  )
where

--------------------------------------------------------------------------------

import MiniJuvix.Utils.Prelude

--------------------------------------------------------------------------------

data CommonError
  = MissingVariable
  | QuantityError
  | UnknownError String

data CheckingError
  = ExpectUniverseType
  | ExpectPiType
  | ExpectTensorType
  | ExpectSumType

-- ! TODO add the other possible cases..

data InferingError

data ErasingError

data Error
  = CheckError CheckingError
  | InferError InferingError
  | ErasureError ErasingError
  | CommonError
