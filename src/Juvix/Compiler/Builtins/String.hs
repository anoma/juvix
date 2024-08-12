module Juvix.Compiler.Builtins.String where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkString :: (Members '[Error ScoperError] r) => AxiomDef -> Sem r ()
checkString d = do
  unless (isSmallUniverse' (d ^. axiomType)) $
    builtinsErrorText (getLoc d) "String should be in the small universe"

checkStringPrint :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkStringPrint f = do
  string_ <- getBuiltinNameScoper (getLoc f) BuiltinString
  io <- getBuiltinNameScoper (getLoc f) BuiltinIO
  unless (f ^. axiomType === (string_ --> io)) $
    builtinsErrorText (getLoc f) "String print has the wrong type signature"

checkIOReadline :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkIOReadline d = do
  string_ <- getBuiltinNameScoper (getLoc d) BuiltinString
  io <- getBuiltinNameScoper (getLoc d) BuiltinIO
  unless (((string_ --> io) --> io) === d ^. axiomType) $
    builtinsErrorText (getLoc d) "readline must be of type (string -> IO) -> IO"

checkNatToString :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkNatToString f = do
  string_ <- getBuiltinNameScoper (getLoc f) BuiltinString
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  unless (f ^. axiomType === (nat --> string_)) $
    builtinsErrorText (getLoc f) "natToString has the wrong type signature"

checkStringToNat :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkStringToNat f = do
  string_ <- getBuiltinNameScoper (getLoc f) BuiltinString
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  unless (f ^. axiomType === (string_ --> nat)) $
    builtinsErrorText (getLoc f) "stringToNat has the wrong type signature"

checkStringConcat :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkStringConcat f = do
  string_ <- getBuiltinNameScoper (getLoc f) BuiltinString
  unless (f ^. axiomType === (string_ --> string_ --> string_)) $
    builtinsErrorText (getLoc f) "++str has the wrong type signature"

checkStringEq :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkStringEq f = do
  string_ <- getBuiltinNameScoper (getLoc f) BuiltinString
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  unless (f ^. axiomType === (string_ --> string_ --> bool_)) $
    builtinsErrorText (getLoc f) "string equality has the wrong type signature"
