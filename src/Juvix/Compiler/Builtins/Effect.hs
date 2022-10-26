module Juvix.Compiler.Builtins.Effect
  ( module Juvix.Compiler.Builtins.Effect,
  )
where

import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Builtins.Error
import Juvix.Prelude

data Builtins m a where
  GetBuiltinName' :: Interval -> BuiltinPrim -> Builtins m Name
  RegisterBuiltin' :: BuiltinPrim -> Name -> Builtins m ()

makeSem ''Builtins

registerBuiltin :: (IsBuiltin a, Member Builtins r) => a -> Name -> Sem r ()
registerBuiltin = registerBuiltin' . toBuiltinPrim

getBuiltinName :: (IsBuiltin a, Member Builtins r) => Interval -> a -> Sem r Name
getBuiltinName i = getBuiltinName' i . toBuiltinPrim

data BuiltinsState = BuiltinsState
  { _builtinsTable :: HashMap BuiltinPrim Name,
    _builtinsNameTable :: HashMap Name BuiltinPrim
  }

makeLenses ''BuiltinsState

iniState :: BuiltinsState
iniState = BuiltinsState mempty mempty

re :: forall r a. Member (Error JuvixError) r => Sem (Builtins ': r) a -> Sem (State BuiltinsState ': r) a
re = reinterpret $ \case
  GetBuiltinName' i b -> fromMaybeM notDefined (gets (^. builtinsTable . at b))
    where
      notDefined :: Sem (State BuiltinsState : r) x
      notDefined =
        throw $
          JuvixError
            NotDefined
              { _notDefinedBuiltin = b,
                _notDefinedLoc = i
              }
  -- GetBuiltin n -> gets (^. builtinsNameTable . at n)
  RegisterBuiltin' b n -> do
    s <- gets (^. builtinsTable . at b)
    case s of
      Nothing -> do
        modify (over builtinsTable (set (at b) (Just n)))
        modify (over builtinsNameTable (set (at n) (Just b)))
      Just {} -> alreadyDefined
    where
      alreadyDefined :: Sem (State BuiltinsState : r) x
      alreadyDefined =
        throw $
          JuvixError
            AlreadyDefined
              { _alreadyDefinedBuiltin = b,
                _alreadyDefinedLoc = getLoc n
              }

runBuiltins :: Member (Error JuvixError) r => BuiltinsState -> Sem (Builtins ': r) a -> Sem r (BuiltinsState, a)
runBuiltins s = runState s . re
