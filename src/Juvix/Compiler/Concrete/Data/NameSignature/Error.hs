module Juvix.Compiler.Concrete.Data.NameSignature.Error where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Options (fromGenericOptions)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Prelude

newtype NameSignatureError = ErrDuplicateName DuplicateName
  deriving stock (Show)

instance ToGenericError NameSignatureError where
  genericError = \case
    ErrDuplicateName d -> genericError d

data DuplicateName = DuplicateName
  { _dupNameFirst :: Symbol,
    _dupNameSecond :: Symbol
  }
  deriving stock (Show)

instance ToGenericError DuplicateName where
  genericError DuplicateName {..} = do
    opts <- fromGenericOptions <$> ask
    let _genericErrorLoc = getLoc _dupNameSecond
        _genericErrorMessage :: AnsiText
        _genericErrorMessage =
          prettyError $
            "The symbol" <+> ppCode opts _dupNameFirst <+> "cannot be repeated"
        _genericErrorIntervals = map getLoc [_dupNameFirst, _dupNameSecond]

    return GenericError {..}
