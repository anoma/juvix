module Juvix.Compiler.Core.Transformation.RemoveBuiltins(removeBuiltins) where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Transformation.Base

removeBuiltins :: InfoTable -> InfoTable
removeBuiltins tab@InfoTable {..} = tab{
  _infoIdentifiers = HashMap.filter (\ii -> isNothing (ii ^. identifierBuiltin)) _infoIdentifiers,
  _infoInductives = HashMap.filter (\ii -> isNothing (ii ^. inductiveBuiltin)) _infoInductives,
  _infoConstructors = HashMap.filter (\ci -> isNothing (ci ^. constructorBuiltin)) _infoConstructors
}
