module MiniJuvix.Error
  ( ErrorType (..),
    ErrorLocation (..),
    ErrorReport (..),
    printErrors,
  )
where

--------------------------------------------------------------------------------

import qualified Data.List as List
import qualified Data.Set as Set
import MiniJuvix.Desugaring.Error (DesugaringError)
import MiniJuvix.Pretty
import MiniJuvix.Typing.Error
import MiniJuvix.Utils.Prelude
import qualified Text.Show

--------------------------------------------------------------------------------

data ErrorType
  =
  DError DesugaringError
  | CError CheckingError
  | UnknownError

instance Show ErrorType where
  show e = case e of
    DError de -> show de
    CError te -> show te
    UnknownError -> show ("UnknownError" :: String)

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

data ErrorReport = ErrorReport
  { _errorType :: ErrorType,
    _errorLoc :: ErrorLocation,
    _errorText :: ErrorDescription,
    _errorParentScopes :: [ErrorScope]
  }

instance Show ErrorReport where
  show _ = undefined

--------------------------------------------------------------------------------

printErrors :: Set ErrorReport -> IO ()
printErrors = printList . Set.toList
