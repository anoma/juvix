module Juvix.Compiler.Builtins.Json where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkJsonDef :: forall r. (Members '[Reader BuiltinsTable, Error ScoperError] r) => InductiveDef -> Sem r ()
checkJsonDef d = do
  let err :: forall a. Text -> Sem r a
      err = builtinsErrorText (getLoc d)
  unless (isSmallUniverse' (d ^. inductiveType)) (err "Json should be in the small universe")
  unless (null (d ^. inductiveParameters)) (err "Json should have no type parameters")
  case d ^. inductiveConstructors of
    [constrJsonEmpty, constrJsonArray, constrJsonBool, constrJsonObject, constrJsonNumber, constrJsonString] ->
      checkJsonEmpty constrJsonEmpty
        >> checkJsonArray constrJsonArray
        >> checkJsonBool constrJsonBool
        >> checkJsonObject constrJsonObject
        >> checkJsonNumber constrJsonNumber
        >> checkJsonString constrJsonString
    _ -> err "Json should have exactly six constructors"

checkJsonEmpty :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkJsonEmpty d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  json_ <- getBuiltinNameScoper (getLoc d) BuiltinJson
  let cty = json_
  unless (ty === cty) $
    builtinsErrorText (getLoc d) "json empty constructor has the wrong type"

checkJsonArray :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkJsonArray d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  json_ <- getBuiltinNameScoper (getLoc d) BuiltinJson
  list_ <- getBuiltinNameScoper (getLoc d) BuiltinList
  let cty = list_ @@ json_ --> json_
  unless (ty === cty) $
    builtinsErrorText (getLoc d) "json array constructor has the wrong type"

checkJsonBool :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkJsonBool d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  json_ <- getBuiltinNameScoper (getLoc d) BuiltinJson
  bool_ <- getBuiltinNameScoper (getLoc d) BuiltinBool
  let cty = bool_ --> json_
  unless (ty === cty) $
    builtinsErrorText (getLoc d) "json bool constructor has the wrong type"

checkJsonObject :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkJsonObject d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  json_ <- getBuiltinNameScoper (getLoc d) BuiltinJson
  list_ <- getBuiltinNameScoper (getLoc d) BuiltinList
  pair_ <- getBuiltinNameScoper (getLoc d) BuiltinPair
  string_ <- getBuiltinNameScoper (getLoc d) BuiltinString
  let cty = list_ @@ (pair_ @@ string_ @@ json_) --> json_
  unless (ty === cty) $
    builtinsErrorText (getLoc d) "json object constructor has the wrong type"

checkJsonNumber :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkJsonNumber d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  json_ <- getBuiltinNameScoper (getLoc d) BuiltinJson
  int_ <- getBuiltinNameScoper (getLoc d) BuiltinInt
  let cty = int_ --> json_
  unless (ty === cty) $
    builtinsErrorText (getLoc d) "json number constructor has the wrong type"

checkJsonString :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkJsonString d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  json_ <- getBuiltinNameScoper (getLoc d) BuiltinJson
  string_ <- getBuiltinNameScoper (getLoc d) BuiltinString
  let cty = string_ --> json_
  unless (ty === cty) $
    builtinsErrorText (getLoc d) "json string constructor has the wrong type"
