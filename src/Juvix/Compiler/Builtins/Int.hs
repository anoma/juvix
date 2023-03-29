module Juvix.Compiler.Builtins.Int where

import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Builtins.Effect
import Juvix.Prelude

registerIntDef :: Member Builtins r => InductiveDef -> Sem r ()
registerIntDef d = do
  unless (null (d ^. inductiveParameters)) (error "Int should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Int should be in the small universe")
  registerBuiltin BuiltinInt (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c1, c2] -> registerIntCtor BuiltinIntOfNat c1 >> registerIntCtor BuiltinIntNegSuc c2
    _ -> error "Int should have exactly two constructors"

registerIntCtor :: (Member Builtins r) => BuiltinConstructor -> InductiveConstructorDef -> Sem r ()
registerIntCtor ctor d@InductiveConstructorDef {..} = do
  let ctorName = _constructorName
      ty = _constructorType
      loc = getLoc d
  int <- getBuiltinName loc BuiltinInt
  nat <- getBuiltinName loc BuiltinNat
  unless (ty === (nat --> int)) (error (ctorName ^. nameText <> " has the wrong type"))
  registerBuiltin ctor ctorName
