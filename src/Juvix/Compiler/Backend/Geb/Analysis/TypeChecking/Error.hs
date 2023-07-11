module Juvix.Compiler.Backend.Geb.Analysis.TypeChecking.Error where

import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty

-- | Errors that can occur during type checking / inference
data CheckingError
  = CheckingErrorTypeMismatch TypeMismatch
  | CheckingErrorExpectedType ExpectedType
  | CheckingErrorLackOfInformation LackOfInformation
  | CheckingErrorWrongObject WrongObject
  deriving stock (Show, Eq)

data TypeMismatch = TypeMismatch
  { _typeMismatchExpected :: Object,
    _typeMismatchActual :: Object
  }
  deriving stock (Show, Eq)

data ExpectedType = ExpectedType
  { _expectedTypeObject :: Object,
    _expectedTypeKind :: Text
  }
  deriving stock (Show, Eq)

data LackOfInformation = LackOfInformation
  { _lackOfInformationMorphism :: Maybe Morphism,
    _lacOfInformationHelperObject :: Maybe Object,
    _lackOfInformationMessage :: String
  }
  deriving stock (Show, Eq)

data WrongObject = WrongObject
  { _wrongObjectExpected :: Maybe Object,
    _wrongObjectActual :: Maybe Object,
    _wrongObjectMorphism :: Morphism,
    _wrongObjectMessage :: String
  }
  deriving stock (Show, Eq)

makeLenses ''TypeMismatch
makeLenses ''ExpectedType
makeLenses ''LackOfInformation
makeLenses ''WrongObject

instance ToGenericError TypeMismatch where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = defaultLoc,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [defaultLoc]
            }
        where
          opts' = fromGenericOptions opts
          expected = e ^. typeMismatchExpected
          actual = e ^. typeMismatchActual
          msg =
            "Object:"
              <> line
              <> ppCode' opts' actual
              <> line
              <> "is expected to be equal to:"
              <> line
              <> ppCode' opts' expected

instance ToGenericError ExpectedType where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = defaultLoc,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [defaultLoc]
            }
        where
          opts' = fromGenericOptions opts
          expected = e ^. expectedTypeObject
          msg =
            "Expected "
              <> pretty (e ^. expectedTypeKind)
              <> ", got:"
              <> line
              <> ppCode' opts' expected

instance ToGenericError LackOfInformation where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = defaultLoc,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [defaultLoc]
            }
        where
          opts' = fromGenericOptions opts
          morph = e ^. lackOfInformationMorphism
          obj = e ^. lacOfInformationHelperObject
          msg =
            "Lack of information:"
              <> line
              <> indent' (pretty (e ^. lackOfInformationMessage))
              <> case morph of
                Nothing -> mempty
                Just m ->
                  line
                    <> "The morphism:"
                    <> line
                    <> indent' (ppCode' opts' m)
              <> case obj of
                Nothing -> mempty
                Just o ->
                  line
                    <> "The object:"
                    <> line
                    <> indent' (ppCode' opts' o)

instance ToGenericError WrongObject where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = defaultLoc,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [defaultLoc]
            }
        where
          opts' = fromGenericOptions opts
          msg =
            pretty (e ^. wrongObjectMessage)
              <> line
              <> "The morphism:"
              <> line
              <> indent' (ppCode' opts' (e ^. wrongObjectMorphism))
              <> case e ^. wrongObjectExpected of
                Nothing -> mempty
                Just o ->
                  line
                    <> "The expected object:"
                    <> line
                    <> indent' (ppCode' opts' o)
              <> case e ^. wrongObjectActual of
                Nothing -> mempty
                Just o ->
                  line
                    <> "The actual object:"
                    <> line
                    <> indent' (ppCode' opts' o)

instance ToGenericError CheckingError where
  genericError = \case
    CheckingErrorTypeMismatch e -> genericError e
    CheckingErrorExpectedType e -> genericError e
    CheckingErrorLackOfInformation e -> genericError e
    CheckingErrorWrongObject e -> genericError e

mockFile :: Path Abs File
mockFile = $(mkAbsFile "/geb-checking-error")

defaultLoc :: Interval
defaultLoc = singletonInterval (mkInitialLoc mockFile)
