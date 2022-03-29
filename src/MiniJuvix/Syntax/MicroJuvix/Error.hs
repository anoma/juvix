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
 = ErrTooManyPatterns
 | ErrWrongConstructorType WrongConstructorType
 | ErrConstructorAppArgs
 | ErrWrongType
 | ErrExpectedFunctionType

prettyT :: Text -> Doc Eann
prettyT = pretty

ppTypeCheckerError :: TypeCheckerError -> Doc Eann
ppTypeCheckerError = \case
  ErrWrongConstructorType e -> ppError e
  ErrTooManyPatterns -> prettyT "too many patterns"
  ErrConstructorAppArgs -> prettyT "constructor has wrong args"
  ErrWrongType -> prettyT "wrong type"
  ErrExpectedFunctionType -> prettyT "expected function type"

docStream :: TypeCheckerError -> SimpleDocStream Eann
docStream = layoutPretty defaultLayoutOptions . ppTypeCheckerError

instance JuvixError TypeCheckerError where
  renderAnsiText :: TypeCheckerError -> Text
  renderAnsiText = renderAnsi . docStream

  renderText :: TypeCheckerError -> Text
  renderText = P.renderText . docStream
