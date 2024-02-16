module Juvix.Compiler.Builtins.Field where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

registerField :: (Member Builtins r) => AxiomDef -> Sem r ()
registerField d = do
  unless (isSmallUniverse' (d ^. axiomType)) (error "String should be in the small universe")
  registerBuiltin BuiltinField (d ^. axiomName)

registerFieldEq :: (Member Builtins r) => AxiomDef -> Sem r ()
registerFieldEq f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  unless (f ^. axiomType === (field_ --> field_ --> bool_)) (error "field equality has the wrong type signature")
  registerBuiltin BuiltinFieldEq (f ^. axiomName)

registerFieldAdd :: (Member Builtins r) => AxiomDef -> Sem r ()
registerFieldAdd f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_)) (error "field addition has the wrong type signature")
  registerBuiltin BuiltinFieldAdd (f ^. axiomName)

registerFieldSub :: (Member Builtins r) => AxiomDef -> Sem r ()
registerFieldSub f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_)) (error "field subtraction has the wrong type signature")
  registerBuiltin BuiltinFieldSub (f ^. axiomName)

registerFieldMul :: (Member Builtins r) => AxiomDef -> Sem r ()
registerFieldMul f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_)) (error "field multiplication has the wrong type signature")
  registerBuiltin BuiltinFieldMul (f ^. axiomName)

registerFieldDiv :: (Member Builtins r) => AxiomDef -> Sem r ()
registerFieldDiv f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_)) (error "field division has the wrong type signature")
  registerBuiltin BuiltinFieldDiv (f ^. axiomName)
