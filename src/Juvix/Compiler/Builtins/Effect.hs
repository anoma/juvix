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

type BuiltinsTable = HashMap BuiltinPrim Symbol

runBuiltins ::
  forall r a.
  (Member (Error BuiltinsError) r) =>
  BuiltinsTable ->
  Sem (Builtins ': r) a ->
  Sem r (BuiltinsTable, a)
runBuiltins ini = reinterpret (runState ini) $ \case
  GetBuiltinSymbol' i b -> fromMaybeM notDefined (gets @BuiltinsTable (^. at b))
    where
      notDefined :: Sem (State BuiltinsTable ': r) x
      notDefined =
        throw $
          ErrNotDefined
            NotDefined
              { _notDefinedBuiltin = b,
                _notDefinedLoc = i
              }
  RegisterBuiltin' b n -> do
    s <- gets @BuiltinsTable (^. at b)
    case s of
      Nothing -> modify @BuiltinsTable (set (at b) (Just n))
      Just {} -> alreadyDefined
    where
      alreadyDefined :: Sem (State BuiltinsTable ': r) x
      alreadyDefined =
        throw $
          ErrAlreadyDefined
            AlreadyDefined
              { _alreadyDefinedBuiltin = b,
                _alreadyDefinedLoc = getLoc n
              }

evalBuiltins :: (Member (Error BuiltinsError) r) => BuiltinsTable -> Sem (Builtins ': r) a -> Sem r a
evalBuiltins s = fmap snd . runBuiltins s
