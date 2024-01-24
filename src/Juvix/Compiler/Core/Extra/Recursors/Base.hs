{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Compiler.Core.Extra.Recursors.Base
  ( module Juvix.Compiler.Core.Data.BinderList,
    module Juvix.Compiler.Core.Language,
    module Juvix.Compiler.Core.Extra.Recursors.Collector,
    module Juvix.Compiler.Core.Extra.Recursors.Recur,
  )
where

import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Recursors.Collector
import Juvix.Compiler.Core.Extra.Recursors.Generic.Base
import Juvix.Compiler.Core.Extra.Recursors.Recur
import Juvix.Compiler.Core.Language

instance IsNodeChild NodeChild Binder where
  gBindersNum = (^. childBindersNum)
  gBinders = (^. childBinders)

instance IsNodeDetails NodeDetails NodeChild where
  gChildren = (^. nodeChildren)

instance IsNode Node NodeDetails NodeChild Binder where
  gDestruct = destruct
  gReassemble = reassembleDetails
  gChild = (^. childNode)
