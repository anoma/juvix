module Juvix.Core.Prelude
  ( module Juvix.Core.Builtins,
    Info,
    Key,
    module Juvix.Prelude,
    module Juvix.Prelude.Loc,
    module Juvix.Syntax.Abstract.Name,
    Location,
  )
where

import Juvix.Core.Builtins
import Juvix.Core.Info (Info, Key)
import Juvix.Prelude
import Juvix.Prelude.Loc
import Juvix.Syntax.Abstract.Name

type Location = Interval
