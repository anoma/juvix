module Juvix.Compiler.Core.Language.Base
  ( Info,
    Key,
    IsInfo,
    module Juvix.Compiler.Core.Language.Builtins,
    module Juvix.Prelude,
    module Juvix.Compiler.Abstract.Data.Name,
    module Juvix.Compiler.Core.Language.Base,
  )
where

import Juvix.Compiler.Abstract.Data.Name
import Juvix.Compiler.Core.Info (Info, IsInfo, Key)
import Juvix.Compiler.Core.Language.Builtins
import Juvix.Prelude

type Location = Interval

-- | Consecutive symbol IDs for reachable user functions.
type Symbol = Word

-- | Tag of a constructor, uniquely identifying it. Tag values are consecutive and
-- separate from symbol IDs. We might need fixed special tags in Core for common
-- "builtin" constructors, e.g., unit, nat, lists, pairs, so that the code
-- generator can treat them specially.
data Tag = BuiltinTag BuiltinDataTag | UserTag Word
  deriving stock (Eq, Generic)

instance Hashable Tag

-- | de Bruijn index
type Index = Int

-- | de Bruijn level (reverse de Bruijn index)
type Level = Int
