module Juvix.Compiler.Builtins.ByteArray where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkByteArray :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkByteArray d = do
  unless (isSmallUniverse' (d ^. axiomType)) (builtinsErrorText (getLoc d) "ByteArray should be in the small universe")

checkByteArrayFromListByte :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkByteArrayFromListByte d = do
  let loc = getLoc d
  byte_ <- getBuiltinNameScoper loc BuiltinByte
  list_ <- getBuiltinNameScoper loc BuiltinList
  byteArray <- getBuiltinNameScoper loc BuiltinByteArray
  unless (d ^. axiomType == (list_ @@ byte_ --> byteArray))
    $ builtinsErrorText (getLoc d) "bytearray-from-list-byte has the wrong type"

checkByteArrayLength :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkByteArrayLength d = do
  let loc = getLoc d
  byteArray <- getBuiltinNameScoper loc BuiltinByteArray
  nat_ <- getBuiltinNameScoper loc BuiltinNat
  unless (d ^. axiomType == (byteArray --> nat_))
    $ builtinsErrorText (getLoc d) "bytearray-length has the wrong type"
