module Juvix.Compiler.Builtins.String where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkString :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkString d = do
  unless (isSmallUniverse' (d ^. axiomType)) $
    builtinsErrorText (getLoc d) "String should be in the small universe"

checkStringPrint :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkStringPrint f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  io <- getBuiltinName (getLoc f) BuiltinIO
  unless (f ^. axiomType === (string_ --> io)) $
    builtinsErrorText (getLoc f) "String print has the wrong type signature"

checkIOReadline :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => AxiomDef -> Sem r ()
checkIOReadline d = do
  string_ <- getBuiltinName (getLoc d) BuiltinString
  io <- getBuiltinName (getLoc d) BuiltinIO
  unless (((string_ --> io) --> io) === d ^. axiomType) $
    builtinsErrorText (getLoc d) "readline must be of type (string -> IO) -> IO"

checkNatToString :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkNatToString f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  nat <- getBuiltinName (getLoc f) BuiltinNat
  unless (f ^. axiomType === (nat --> string_)) $
    builtinsErrorText (getLoc f) "natToString has the wrong type signature"

checkStringToNat :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkStringToNat f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  nat <- getBuiltinName (getLoc f) BuiltinNat
  unless (f ^. axiomType === (string_ --> nat)) $
    builtinsErrorText (getLoc f) "stringToNat has the wrong type signature"

checkStringConcat :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkStringConcat f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  unless (f ^. axiomType === (string_ --> string_ --> string_)) $
    builtinsErrorText (getLoc f) "++str has the wrong type signature"

checkStringEq :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkStringEq f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  unless (f ^. axiomType === (string_ --> string_ --> bool_)) $
    builtinsErrorText (getLoc f) "string equality has the wrong type signature"
