module Juvix.Compiler.Tree.Transformation.CheckNoByteArray where

import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Extra.Recursors
import Juvix.Compiler.Tree.Transformation.Base

checkNoAnoma :: forall r. (Member (Error TreeError) r) => InfoTable -> Sem r ()
checkNoAnoma = walkT checkNode
  where
    checkNode :: Symbol -> Node -> Sem r ()
    checkNode _ = \case
      ByteArray NodeByteArray {..} -> case _nodeByteArrayOpcode of
        OpByteArraySize -> unsupportedErr "OpByteArraySize"
        OpByteArraYFromListUInt8 -> unsupportedErr "OpByteArrayFromListUInt8"
        where
          unsupportedErr :: Text -> Sem r ()
          unsupportedErr opName =
            throw
              TreeError
                { _treeErrorMsg = opName <> " is unsupported",
                  _treeErrorLoc = _nodeAnomaInfo ^. nodeInfoLocation
                }
      _ -> return ()
