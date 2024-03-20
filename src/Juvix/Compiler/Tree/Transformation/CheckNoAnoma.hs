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
      Unop NodeUnop {..} -> case _nodeUnopOpcode of
        OpAnomaGet ->
          throw
            TreeError
              { _treeErrorMsg = "OpAnomaGet is unsupported",
                _treeErrorLoc = _nodeUnopInfo ^. nodeInfoLocation
              }
        OpFail -> return ()
        OpTrace -> return ()
        PrimUnop {} -> return ()
      _ -> return ()
