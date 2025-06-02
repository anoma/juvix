module Juvix.Compiler.Builtins.Field where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkField :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkField d = do
  unless (isSmallUniverse' (d ^. axiomType))
    $ builtinsErrorText (getLoc d) "String should be in the small universe"

checkFieldEq :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkFieldEq f = do
  field_ <- getBuiltinNameScoper (getLoc f) BuiltinField
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  unless (f ^. axiomType === (field_ --> field_ --> bool_))
    $ builtinsErrorText (getLoc f) "field equality has the wrong type signature"

checkFieldAdd :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkFieldAdd f = do
  field_ <- getBuiltinNameScoper (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_))
    $ builtinsErrorText (getLoc f) "field addition has the wrong type signature"

checkFieldSub :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkFieldSub f = do
  field_ <- getBuiltinNameScoper (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_))
    $ builtinsErrorText (getLoc f) "field subtraction has the wrong type signature"

checkFieldMul :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkFieldMul f = do
  field_ <- getBuiltinNameScoper (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_))
    $ builtinsErrorText (getLoc f) "field multiplication has the wrong type signature"

checkFieldDiv :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkFieldDiv f = do
  field_ <- getBuiltinNameScoper (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_))
    $ builtinsErrorText (getLoc f) "field division has the wrong type signature"

checkFieldFromInt :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkFieldFromInt f = do
  field_ <- getBuiltinNameScoper (getLoc f) BuiltinField
  int_ <- getBuiltinNameScoper (getLoc f) BuiltinInt
  unless (f ^. axiomType === (int_ --> field_))
    $ builtinsErrorText (getLoc f) "integer to field conversion has the wrong type signature"

checkFieldToNat :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkFieldToNat f = do
  field_ <- getBuiltinNameScoper (getLoc f) BuiltinField
  nat_ <- getBuiltinNameScoper (getLoc f) BuiltinNat
  unless (f ^. axiomType === (field_ --> nat_))
    $ builtinsErrorText (getLoc f) "field to nat conversion has the wrong type signature"
