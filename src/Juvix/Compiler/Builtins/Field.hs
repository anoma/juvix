module Juvix.Compiler.Builtins.Field where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkField :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkField d = do
  unless (isSmallUniverse' (d ^. axiomType)) $
    builtinsErrorText (getLoc d) "String should be in the small universe"

checkFieldEq :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkFieldEq f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  unless (f ^. axiomType === (field_ --> field_ --> bool_)) $
    builtinsErrorText (getLoc f) "field equality has the wrong type signature"

checkFieldAdd :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkFieldAdd f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_)) $
    builtinsErrorText (getLoc f) "field addition has the wrong type signature"

checkFieldSub :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkFieldSub f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_)) $
    builtinsErrorText (getLoc f) "field subtraction has the wrong type signature"

checkFieldMul :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkFieldMul f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_)) $
    builtinsErrorText (getLoc f) "field multiplication has the wrong type signature"

checkFieldDiv :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkFieldDiv f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  unless (f ^. axiomType === (field_ --> field_ --> field_)) $
    builtinsErrorText (getLoc f) "field division has the wrong type signature"

checkFieldFromInt :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkFieldFromInt f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  int_ <- getBuiltinName (getLoc f) BuiltinInt
  unless (f ^. axiomType === (int_ --> field_)) $
    builtinsErrorText (getLoc f) "integer to field conversion has the wrong type signature"

checkFieldToNat :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkFieldToNat f = do
  field_ <- getBuiltinName (getLoc f) BuiltinField
  nat_ <- getBuiltinName (getLoc f) BuiltinNat
  unless (f ^. axiomType === (field_ --> nat_)) $
    builtinsErrorText (getLoc f) "field to nat conversion has the wrong type signature"
