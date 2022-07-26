module Juvix.Core.Type where

import Juvix.Prelude
import Juvix.Syntax.Abstract.Name

data Type = Atomic Atom | Fun Type Type | Universe

data Atom = Atom
  { _atomHead :: Name,
    _atomArgs :: [Type]
  }

makeLenses ''Atom

-- destructs a type into the target and the arguments (left-to-right)
destructType :: Type -> (Type, [Type])
destructType ty = case ty of
  Fun l r -> let (tgt, args) = destructType r in (tgt, l:args)
  _ -> (ty, [])

getTarget :: Type -> Type
getTarget = fst . destructType

getArgs :: Type -> [Type]
getArgs = snd . destructType
