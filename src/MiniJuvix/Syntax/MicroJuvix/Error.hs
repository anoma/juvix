module MiniJuvix.Syntax.MicroJuvix.Error
  ( module MiniJuvix.Syntax.MicroJuvix.Error,
    module MiniJuvix.Syntax.MicroJuvix.Error.Pretty,
    module MiniJuvix.Syntax.MicroJuvix.Error.Types,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty
import qualified MiniJuvix.Syntax.MicroJuvix.Error.Pretty as P
import MiniJuvix.Syntax.MicroJuvix.Error.Types
import Prettyprinter

data TypeCheckerError
 = ErrTooManyPatterns TooManyPatterns
 | ErrWrongConstructorType WrongConstructorType
 | ErrWrongConstructorAppArgs WrongConstructorAppArgs
 | ErrWrongType WrongType
 | ErrExpectedFunctionType ExpectedFunctionType

ppTypeCheckerError :: TypeCheckerError -> Doc Eann
ppTypeCheckerError = \case
  ErrWrongConstructorType e -> ppError e
  ErrTooManyPatterns e -> ppError e
  ErrWrongConstructorAppArgs e -> ppError e
  ErrWrongType e -> ppError e
  ErrExpectedFunctionType e -> ppError e

docStream :: TypeCheckerError -> SimpleDocStream Eann
docStream = layoutPretty defaultLayoutOptions . ppTypeCheckerError

instance JuvixError TypeCheckerError where
  renderAnsiText :: TypeCheckerError -> Text
  renderAnsiText = renderAnsi . docStream

  renderText :: TypeCheckerError -> Text
  renderText = P.renderText . docStream
