module Repl.Value where

import Base
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Language.Value qualified as Core
import Juvix.Extra.Strings qualified as Str

mkInteger :: Integer -> Core.Value
mkInteger = Core.ValueConstant . Core.ConstInteger

mkBool :: Bool -> Core.Value
mkBool b =
  Core.ValueConstrApp
    ( Core.ConstrApp
        { _constrAppName = name,
          _constrAppFixity = Irrelevant Nothing,
          _constrAppArgs = []
        }
    )
  where
    name :: Text
    name = case b of
      True -> Str.true
      False -> Str.false
