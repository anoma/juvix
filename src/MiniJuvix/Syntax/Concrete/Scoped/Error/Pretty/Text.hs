module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Text where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Base
import Prettyprinter
import Prettyprinter.Render.Text

renderText :: SimpleDocStream Eann -> Text
renderText = renderStrict
