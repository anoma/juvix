module MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Text where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Base
import Prettyprinter
import Prettyprinter.Render.Text

renderText :: SimpleDocStream Eann -> Text
renderText = renderStrict
