module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking.Error where

import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty

data CheckingError
  = CheckingErrorTypeMismatch TypeMismatch
  | CheckingErrorLackOfInformation LackOfInformation
  deriving stock (Show, Eq)

data TypeMismatch = TypeMismatch
  { _typeMismatchExpected :: Object,
    _typeMismatchActual :: Object,
    _typeMismatchMorphism :: Morphism
  }
  deriving stock (Show, Eq)

data LackOfInformation = LackOfInformation
  { _lackOfInformationMorphism :: Maybe Morphism,
    _lacOfInformationHelperObject :: Maybe Object,
    _lackOfInformationMessage :: String
  }
  deriving stock (Show, Eq)

makeLenses ''TypeMismatch
makeLenses ''LackOfInformation

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
          morph = e ^. typeMismatchMorphism
          expected = e ^. typeMismatchExpected
          actual = e ^. typeMismatchActual
          msg =
            "The"
              <+> ppCode' opts' morph
              <+> "has object:"
                <> line
                <> indent' (ppCode' opts' actual)
                <> line
                <> "but is expected to have as object:"
                <> line
                <> indent' (ppCode' opts' expected)

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

instance ToGenericError CheckingError where
  genericError (CheckingErrorTypeMismatch e) = genericError e
  genericError (CheckingErrorLackOfInformation e) = genericError e

-- TODO: use the real file
mockFile :: Path Abs File
mockFile = $(mkAbsFile "/mockfile")

defaultLoc :: Interval
defaultLoc = singletonInterval (mkInitialLoc mockFile)