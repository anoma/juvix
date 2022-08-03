module Juvix.Core.Language.Base
  ( Info,
    Key,
    IsInfo,
    module Juvix.Core.Language.Builtins,
    module Juvix.Prelude,
    module Juvix.Prelude.Loc,
    module Juvix.Syntax.Abstract.Name,
    Location,
    Symbol,
    Tag,
  )
where

import Juvix.Core.Language.Builtins
import Juvix.Core.Language.Info (Info, IsInfo, Key)
import Juvix.Prelude
import Juvix.Prelude.Loc
import Juvix.Syntax.Abstract.Name

type Location = Interval

-- Consecutive symbol IDs for reachable user functions.
type Symbol = Word

-- Tag of a constructor, uniquely identifying it. Tag values are consecutive and
-- separate from symbol IDs. We might need fixed special tags in Core for common
-- "builtin" constructors, e.g., unit, nat, lists, pairs, so that the code
-- generator can treat them specially.
data Tag = BuiltinTag BuiltinDataTag | UserTag Word
  deriving stock (Eq, Generic)

instance Hashable Tag
