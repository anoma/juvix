module Juvix.Core.Prelude
  ( module Juvix.Core.Builtins,
    Info,
    Key,
    module Juvix.Prelude,
    module Juvix.Prelude.Loc,
    module Juvix.Syntax.Abstract.Name,
    Location,
    hd,
    tl,
  )
where

import Data.List qualified as List
import Juvix.Core.Builtins
import Juvix.Core.Info (Info, Key)
import Juvix.Prelude
import Juvix.Prelude.Loc
import Juvix.Syntax.Abstract.Name

type Location = Interval

hd :: [a] -> a
hd = List.head

tl :: [a] -> [a]
tl = List.tail
