module MiniJuvix.Error
  ( ErrorType (..),
    ErrorLocation (..),
  )
where

--------------------------------------------------------------------------------

import MiniJuvix.Desugaring.Error (DesugaringError)
import MiniJuvix.Parsing.Error (ParsingError)
import MiniJuvix.Pretty
import MiniJuvix.Typing.Error (TypingError (..))
import MiniJuvix.Utils.Prelude

--------------------------------------------------------------------------------

data ErrorType
  = PError ParsingError
  | DError DesugaringError
  | TError TypingError
  | UnknownError

instance Show ErrorType where
  show e = case e of
    PError pe -> show pe
    DError de -> show de
    TError te -> show pe
    UnknownError -> show "UnknownError"

--------------------------------------------------------------------------------

type Row = Int

type Col = Int

type Loc = (String, Row, Col)

newtype ErrorLocation = ErrorLocation (Maybe (Loc, Loc))
  deriving stock (Eq, Ord, Show)

{- TODO: I don't know yet how to deal with scope. But the errors
should be printed with some information about the enviroment.
-}
data Scope

--------------------------------------------------------------------------------

type ErrorDescription = Text

type ErrorScope = Maybe Scope

data Error
  = Error
      { _errorType :: ErrorType,
        _errorLoc :: ErrorLocation,
        _errorText :: ErrorDescription,
        _errorParentScopes :: [ErrorScope]
      }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

logErrs :: Set Error -> IO ()
logErrs = printList . L.sort . S.toList
