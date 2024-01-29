module Juvix.Compiler.Tree.Extra.Recursors.Recur
  ( module Juvix.Compiler.Tree.Extra.Recursors.Recur,
    module Juvix.Compiler.Core.Extra.Recursors.Generic.Recur,
  )
where

import Juvix.Compiler.Core.Extra.Recursors.Generic.Recur (pattern End, pattern End', pattern Recur, pattern Recur')
import Juvix.Compiler.Core.Extra.Recursors.Generic.Recur qualified as G
import Juvix.Compiler.Core.Language

type Recur' c = G.Recur' Node c

type Recur = G.Recur Node
