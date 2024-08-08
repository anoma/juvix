module Juvix.Compiler.Builtins.Effect
  ( module Juvix.Compiler.Builtins.Effect,
    module Juvix.Compiler.Builtins.Error,
  )
where

import Juvix.Compiler.Builtins.Error
import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Prelude

data Builtins :: Effect where
  GetBuiltinSymbol' :: Interval -> BuiltinPrim -> Builtins m Symbol
  RegisterBuiltin' :: BuiltinPrim -> Symbol -> Builtins m ()

makeSem ''Builtins

registerBuiltin :: (IsBuiltin a, Member Builtins r) => a -> Symbol -> Sem r ()
registerBuiltin = registerBuiltin' . toBuiltinPrim

getBuiltinSymbol :: (IsBuiltin a, Member Builtins r) => Interval -> a -> Sem r Symbol
getBuiltinSymbol i = getBuiltinSymbol' i . toBuiltinPrim

newtype BuiltinsState = BuiltinsState
  { _builtinsTable :: HashMap BuiltinPrim Symbol
  }

makeLenses ''BuiltinsState

iniBuiltins :: BuiltinsState
iniBuiltins = BuiltinsState mempty

runBuiltins :: forall r a. (Member (Error BuiltinsError) r) => BuiltinsState -> Sem (Builtins ': r) a -> Sem r (BuiltinsState, a)
runBuiltins ini = reinterpret (runState ini) $ \case
  GetBuiltinSymbol' i b -> fromMaybeM notDefined (gets (^. builtinsTable . at b))
    where
      notDefined :: Sem (State BuiltinsState ': r) x
      notDefined =
        throw $
          ErrNotDefined
            NotDefined
              { _notDefinedBuiltin = b,
                _notDefinedLoc = i
              }
  RegisterBuiltin' b n -> do
    s <- gets (^. builtinsTable . at b)
    case s of
      Nothing -> do
        modify (over builtinsTable (set (at b) (Just n)))
      Just {} -> alreadyDefined
    where
      alreadyDefined :: Sem (State BuiltinsState ': r) x
      alreadyDefined =
        throw $
          ErrAlreadyDefined
            AlreadyDefined
              { _alreadyDefinedBuiltin = b,
                _alreadyDefinedLoc = getLoc n
              }

evalBuiltins :: (Member (Error BuiltinsError) r) => BuiltinsState -> Sem (Builtins ': r) a -> Sem r a
evalBuiltins s = fmap snd . runBuiltins s
