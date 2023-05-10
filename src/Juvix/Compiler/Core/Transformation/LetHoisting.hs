-- Moves al let expressions at the top, just after the top lambdas. This
-- transformation assumes there are no LetRecs, Lambdas (other than the ones at
-- the top), nor Match.
module Juvix.Compiler.Core.Transformation.LetHoisting
  ( module Juvix.Compiler.Core.Transformation.LetHoisting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.ComputeTypeInfo (computeNodeType)

hoistBinder :: Members '[Reader OnlyLetRec, InfoTableBuilder] r => BinderList Binder -> Binder -> Sem r Binder
hoistBinder bl = traverseOf binderType (hoistNode bl)

type OnlyLetRec = Bool

hoistNode :: forall r. Members '[Reader OnlyLetRec, InfoTableBuilder] r => BinderList Binder -> Node -> Sem r Node
hoistNode aboveBl top = undefined
