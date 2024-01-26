{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Compiler.Tree.Extra.Recursors.Base
  ( module Juvix.Compiler.Core.Data.BinderList,
    module Juvix.Compiler.Tree.Language,
    module Juvix.Compiler.Core.Extra.Recursors.Generic.Collector,
    module Juvix.Compiler.Tree.Extra.Recursors.Recur,
  )
where

import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Extra.Recursors.Generic.Base
import Juvix.Compiler.Core.Extra.Recursors.Generic.Collector
import Juvix.Compiler.Tree.Extra.Base
import Juvix.Compiler.Tree.Extra.Recursors.Recur
import Juvix.Compiler.Tree.Language

instance IsNodeChild NodeChild TempVar where
  gBindersNum = fromEnum . isJust . (^. childTempVar)
  gBinders = toList . (^. childTempVar)

instance IsNodeDetails NodeDetails NodeChild where
  gChildren = (^. nodeChildren)

instance IsNode Node NodeDetails NodeChild TempVar where
  gDestruct = destruct
  gReassemble = reassembleDetails
  gChild = (^. childNode)
