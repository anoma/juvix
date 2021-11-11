module MiniJuvix.Typing.Error 
  (TypingError(..))
  where

--------------------------------------------------------------------------------

-- Specific error related to an algorithm
data CheckingError
data InferingError
data ErasingError


data TypingError
  = CheckError CheckingError
  | InferError InferingError
  | ErasureError ErasingError
  | UnknownTypingError
