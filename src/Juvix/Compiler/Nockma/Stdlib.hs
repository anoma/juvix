module Juvix.Compiler.Nockma.Stdlib where

import Data.FileEmbed qualified as FE
import Juvix.Compiler.Nockma.Translation.FromSource
import Juvix.Prelude.Base

stdlib :: Term Natural
stdlib =
  fromRight impossible
    $ parseText
    $ decodeUtf8 $(FE.makeRelativeToProject "runtime/nockma/stdlib.nockma" >>= FE.embedFile)
