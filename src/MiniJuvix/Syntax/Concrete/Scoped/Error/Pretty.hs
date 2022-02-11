module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty (
  module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Base,
  module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Ansi,
  module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty
  ) where

import MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Base
import MiniJuvix.Utils.Prelude
import Prettyprinter
import MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Ansi
import MiniJuvix.Syntax.Concrete.Scoped.Error.Types (ScopeError)

printErrorAnsi :: ScopeError -> IO ()
printErrorAnsi = hPutStrLn stderr . renderErrorAnsi

renderErrorAnsi :: ScopeError -> Text
renderErrorAnsi = renderAnsi . docStream

docStream :: ScopeError -> SimpleDocStream Eann
docStream = layoutPretty defaultLayoutOptions . ppScopeError
