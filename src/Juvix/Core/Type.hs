module Juvix.Core.Type where

import Juvix.Core.Prelude

data Type = Atomic Atom | Fun Type Type | Universe

data Atom = Atom
  { _atomHead :: Name,
    _atomArgs :: [Type]
  }

makeLenses ''Atom

-- unfold a type into the target and the arguments (left-to-right)
unfoldType :: Type -> (Type, [Type])
unfoldType ty = case ty of
  Fun l r -> let (tgt, args) = unfoldType r in (tgt, l:args)
  _ -> (ty, [])

getTarget :: Type -> Type
getTarget = fst . unfoldType

getArgs :: Type -> [Type]
getArgs = snd . unfoldType
