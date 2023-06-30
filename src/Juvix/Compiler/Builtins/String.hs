module Juvix.Compiler.Builtins.String where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

registerString :: (Member Builtins r) => AxiomDef -> Sem r ()
registerString d = do
  unless (isSmallUniverse' (d ^. axiomType)) (error "String should be in the small universe")
  registerBuiltin BuiltinString (d ^. axiomName)

registerStringPrint :: (Member Builtins r) => AxiomDef -> Sem r ()
registerStringPrint f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  io <- getBuiltinName (getLoc f) BuiltinIO
  unless (f ^. axiomType === (string_ --> io)) (error "String print has the wrong type signature")
  registerBuiltin BuiltinStringPrint (f ^. axiomName)

registerIOReadline :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerIOReadline d = do
  string_ <- getBuiltinName (getLoc d) BuiltinString
  io <- getBuiltinName (getLoc d) BuiltinIO
  unless (((string_ --> io) --> io) === d ^. axiomType) (error "readline must be of type (string -> IO) -> IO")
  registerBuiltin BuiltinIOReadline (d ^. axiomName)

registerNatToString :: (Member Builtins r) => AxiomDef -> Sem r ()
registerNatToString f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  nat <- getBuiltinName (getLoc f) BuiltinNat
  unless (f ^. axiomType === (nat --> string_)) (error "natToString has the wrong type signature")
  registerBuiltin BuiltinNatToString (f ^. axiomName)

registerStringToNat :: (Member Builtins r) => AxiomDef -> Sem r ()
registerStringToNat f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  nat <- getBuiltinName (getLoc f) BuiltinNat
  unless (f ^. axiomType === (string_ --> nat)) (error "stringToNat has the wrong type signature")
  registerBuiltin BuiltinStringToNat (f ^. axiomName)

registerStringConcat :: (Member Builtins r) => AxiomDef -> Sem r ()
registerStringConcat f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  unless (f ^. axiomType === (string_ --> string_ --> string_)) (error "++str has the wrong type signature")
  registerBuiltin BuiltinStringConcat (f ^. axiomName)

registerStringEq :: (Member Builtins r) => AxiomDef -> Sem r ()
registerStringEq f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  unless (f ^. axiomType === (string_ --> string_ --> bool_)) (error "string equality has the wrong type signature")
  registerBuiltin BuiltinStringEq (f ^. axiomName)
