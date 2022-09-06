module Juvix.Compiler.Core.Extra.Recursors.Parameters where

import Juvix.Prelude

data CollectorIni
  = NoIni
  | Ini

data Ctx
  = CtxBinderList
  | CtxBinderNum
  | CtxNone

data Monadic
  = Monadic
  | NonMonadic

data Ret
  = RetRecur
  | RetSimple

data Direction
  = TopDown
  | BottomUp

$(genSingletons [''CollectorIni, ''Ctx, ''Ret, ''Monadic, ''Direction])
