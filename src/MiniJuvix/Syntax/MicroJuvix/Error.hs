module MiniJuvix.Syntax.MicroJuvix.Error
  ( module MiniJuvix.Syntax.MicroJuvix.Error,
    module MiniJuvix.Syntax.MicroJuvix.Error.Pretty,
    module MiniJuvix.Syntax.MicroJuvix.Error.Types,
  )
where

--------------------------------------------------------------------------------

import Data.Text qualified as Text
import MiniJuvix.Prelude qualified as Prelude
import MiniJuvix.Prelude.Base
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty qualified as P
import MiniJuvix.Syntax.MicroJuvix.Error.Types
import Prettyprinter

--------------------------------------------------------------------------------

data TypeCheckerError
  = ErrTooManyPatterns TooManyPatterns
  | ErrWrongConstructorType WrongConstructorType
  | ErrWrongConstructorAppArgs WrongConstructorAppArgs
  | ErrWrongType WrongType
  | ErrExpectedFunctionType ExpectedFunctionType
  deriving stock (Show)

newtype TypeCheckerErrors = TypeCheckerErrors
  { _unTypeCheckerErrors :: NonEmpty TypeCheckerError
  }
  deriving stock (Show)

makeLenses ''TypeCheckerErrors

ppTypeCheckerError :: TypeCheckerError -> Doc Eann
ppTypeCheckerError = \case
  ErrWrongConstructorType e -> ppError e
  ErrTooManyPatterns e -> ppError e
  ErrWrongConstructorAppArgs e -> ppError e
  ErrWrongType e -> ppError e
  ErrExpectedFunctionType e -> ppError e

docStream :: TypeCheckerError -> SimpleDocStream Eann
docStream = layoutPretty defaultLayoutOptions . ppTypeCheckerError

instance Prelude.JuvixError TypeCheckerError where
  renderAnsiText :: TypeCheckerError -> Text
  renderAnsiText = renderAnsi . docStream

  renderText :: TypeCheckerError -> Text
  renderText = P.renderText . docStream

instance Prelude.JuvixError TypeCheckerErrors where
  renderAnsiText :: TypeCheckerErrors -> Text
  renderAnsiText TypeCheckerErrors {..} = (Text.unlines . toList) (fmap Prelude.renderAnsiText _unTypeCheckerErrors)

  renderText :: TypeCheckerErrors -> Text
  renderText TypeCheckerErrors {..} = (Text.unlines . toList) (fmap Prelude.renderText _unTypeCheckerErrors)
