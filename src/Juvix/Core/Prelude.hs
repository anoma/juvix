module Juvix.Core.Prelude
  ( Info,
    Key,
    IsInfo,
    module Juvix.Prelude,
    module Juvix.Prelude.Loc,
    module Juvix.Syntax.Abstract.Name,
    Location
  )
where

import Juvix.Core.Language.Info (Info, Key, IsInfo)
import Juvix.Prelude
import Juvix.Prelude.Loc
import Juvix.Syntax.Abstract.Name

type Location = Interval
