module MiniJuvix.Syntax.MicroJuvix.Error
  ( module MiniJuvix.Syntax.MicroJuvix.Error,
    module MiniJuvix.Syntax.MicroJuvix.Error.Pretty,
    module MiniJuvix.Syntax.MicroJuvix.Error.Types,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ann
import MiniJuvix.Syntax.MicroJuvix.Error.Types
import Prettyprinter
import Prettyprinter.Render.Text qualified as Text

data TypeCheckerError
  = ErrTooManyPatterns TooManyPatterns
  | ErrWrongConstructorType WrongConstructorType
  | ErrWrongConstructorAppArgs WrongConstructorAppArgs
  | ErrWrongType WrongType
  | ErrExpectedFunctionType ExpectedFunctionType
  deriving stock (Show)

ppTypeCheckerError :: TypeCheckerError -> Doc Eann
ppTypeCheckerError = \case
  ErrWrongConstructorType e -> ppError e
  ErrTooManyPatterns e -> ppError e
  ErrWrongConstructorAppArgs e -> ppError e
  ErrWrongType e -> ppError e
  ErrExpectedFunctionType e -> ppError e

docStream :: TypeCheckerError -> SimpleDocStream Eann
docStream = layoutPretty defaultLayoutOptions . ppTypeCheckerError

instance ToGenericError TypeCheckerError where
  genericError :: TypeCheckerError -> Maybe GenericError
  genericError = \case
    ErrTooManyPatterns e -> genericError e
    ErrWrongConstructorType e -> genericError e
    ErrWrongConstructorAppArgs e -> genericError e
    ErrWrongType e -> genericError e
    ErrExpectedFunctionType e -> genericError e

instance JuvixError TypeCheckerError where
  renderAnsiText :: TypeCheckerError -> Text
  renderAnsiText = renderAnsi . docStream

  renderText :: TypeCheckerError -> Text
  renderText = Text.renderStrict . docStream
