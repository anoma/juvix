module Juvix.Compiler.Tree.Transformation.CheckNoAnoma where

import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Extra.Recursors
import Juvix.Compiler.Tree.Transformation.Base

checkNoAnoma :: forall r. (Member (Error TreeError) r) => InfoTable -> Sem r ()
checkNoAnoma = walkT checkNode
  where
    checkNode :: Symbol -> Node -> Sem r ()
    checkNode _ = \case
      Anoma NodeAnoma {..} -> case _nodeAnomaOpcode of
        OpAnomaGet -> unsupportedErr "OpAnomaGet"
        OpAnomaEncode -> unsupportedErr "OpAnomaEncode"
        OpAnomaDecode -> unsupportedErr "OpAnomaDecode"
        OpAnomaVerifyDetached -> unsupportedErr "OpAnomaVerifyDetached"
        OpAnomaSign -> unsupportedErr "OpAnomaSign"
        OpAnomaSignDetached -> unsupportedErr "OpAnomaSignDetached"
        OpAnomaVerifyWithMessage -> unsupportedErr "OpAnomaVerifyWithMessage"
        OpAnomaByteArrayFromAnomaContents -> unsupportedErr "OpAnomaByteArrayFromAnomaContents"
        OpAnomaByteArrayToAnomaContents -> unsupportedErr "OpAnomaByteArrayToAnomaContents"
        OpAnomaSha256 -> unsupportedErr "OpAnomaSha256"
        where
          unsupportedErr :: Text -> Sem r ()
          unsupportedErr opName =
            throw
              TreeError
                { _treeErrorMsg = opName <> " is unsupported",
                  _treeErrorLoc = _nodeAnomaInfo ^. nodeInfoLocation
                }
      _ -> return ()
