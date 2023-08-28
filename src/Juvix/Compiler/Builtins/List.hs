module Juvix.Compiler.Builtins.List where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Prelude

registerListDef :: (Member Builtins r) => InductiveDef -> Sem r ()
registerListDef d = do
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Lists should be in the small universe")
  registerBuiltin BuiltinList (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c1, c2] -> registerNil param c1 >> registerCons param c2
    _ -> error "List should have exactly two constructors"
  where
    param :: VarName
    param = case d ^. inductiveParameters of
      [v] -> v ^. inductiveParamName
      _ -> error "List should have exactly one type parameter"

registerNil :: (Member Builtins r) => VarName -> ConstructorDef -> Sem r ()
registerNil a d@ConstructorDef {..} = do
  let nil = _inductiveConstructorName
      ty = _inductiveConstructorType
  list_ <- getBuiltinName (getLoc d) BuiltinList
  let nilty = list_ @@ a
  unless (ty === nilty) (error $ "nil has the wrong type " <> ppTrace ty <> " | " <> ppTrace nilty)
  registerBuiltin BuiltinListNil nil

registerCons :: (Member Builtins r) => VarName -> ConstructorDef -> Sem r ()
registerCons a d@ConstructorDef {..} = do
  let cons_ = _inductiveConstructorName
      ty = _inductiveConstructorType
  list_ <- getBuiltinName (getLoc d) BuiltinList
  let consty = a --> list_ @@ a --> list_ @@ a
  unless (ty === consty) (error "cons has the wrong type")
  registerBuiltin BuiltinListCons cons_
