module Juvix.Core.Language.Base
  ( Info,
    Key,
    IsInfo,
    module Juvix.Prelude,
    module Juvix.Prelude.Loc,
    module Juvix.Syntax.Abstract.Name,
    Location,
  )
where

import Juvix.Core.Language.Info (Info, IsInfo, Key)
import Juvix.Prelude
import Juvix.Prelude.Loc
import Juvix.Syntax.Abstract.Name

type Location = Interval
