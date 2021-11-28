module MiniJuvix.Typing.Error
  ( Error (..),
    CheckingError (..),
    CommonError (..),
  )
where

--------------------------------------------------------------------------------

import MiniJuvix.Utils.Prelude
import qualified Text.Show

--------------------------------------------------------------------------------

data CommonError
  = MissingVariable
  | QuantityError
  deriving stock (Show)

data CheckingError
  = ExpectUniverseType
  | ExpectPiType
  | ExpectTensorType
  | ExpectSumType
  deriving stock (Show)

-- ! TODO add the other possible cases..

data InferingError = InferingError
  deriving stock (Show)

data ErasingError = ErasingError
  deriving stock (Show)

data Error
  = CheckError CheckingError
  | InferError InferingError
  | ErasureError ErasingError
  | CommonError
  deriving stock (Show)
