module Juvix.Compiler.Builtins.Effect
  ( module Juvix.Compiler.Builtins.Effect,
  )
where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Abstract.Pretty
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

iniBuiltins :: BuiltinsState
iniBuiltins = BuiltinsState mempty mempty

re :: forall r a. (Member (Error JuvixError) r) => Sem (Builtins ': r) a -> Sem (State BuiltinsState ': r) a
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

evalTopBuiltins :: (Member (Error JuvixError) r) => Sem (Builtins ': r) a -> Sem r a
evalTopBuiltins = fmap snd . runTopBuiltins

runTopBuiltins :: (Member (Error JuvixError) r) => Sem (Builtins ': r) a -> Sem r (BuiltinsState, a)
runTopBuiltins = runBuiltins iniBuiltins

runBuiltins :: (Member (Error JuvixError) r) => BuiltinsState -> Sem (Builtins ': r) a -> Sem r (BuiltinsState, a)
runBuiltins s = runState s . re

data FunInfo = FunInfo
  { _funInfoDef :: FunctionDef,
    _funInfoBuiltin :: BuiltinFunction,
    _funInfoSignature :: Expression,
    _funInfoClauses :: [(Expression, Expression)],
    _funInfoFreeVars :: [VarName],
    _funInfoFreeTypeVars :: [VarName]
  }

makeLenses ''FunInfo

registerFun ::
  Members '[Builtins, NameIdGen] r =>
  FunInfo ->
  Sem r ()
registerFun fi = do
  let op = fi ^. funInfoDef . funDefName
      ty = fi ^. funInfoDef . funDefTypeSig
      sig = fi ^. funInfoSignature
  unless ((sig ==% ty) (HashSet.fromList (fi ^. funInfoFreeTypeVars))) (error "builtin has the wrong type signature")
  registerBuiltin (fi ^. funInfoBuiltin) op
  let freeVars = HashSet.fromList (fi ^. funInfoFreeVars)
      a =% b = (a ==% b) freeVars
      clauses :: [(Expression, Expression)]
      clauses =
        [ (clauseLhsAsExpression c, c ^. clauseBody)
          | c <- toList (fi ^. funInfoDef . funDefClauses)
        ]
  case zipExactMay (fi ^. funInfoClauses) clauses of
    Nothing -> error "builtin has the wrong number of clauses"
    Just z -> forM_ z $ \((exLhs, exBody), (lhs, body)) -> do
      unless (exLhs =% lhs) (error "clause lhs does not match")
      unless (exBody =% body) (error $ "clause body does not match " <> ppTrace exBody <> " | " <> ppTrace body)
