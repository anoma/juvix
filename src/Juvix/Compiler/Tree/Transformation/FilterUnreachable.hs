module Juvix.Compiler.Tree.Transformation.FilterUnreachable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Tree.Data.CallGraph
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Prelude

filterUnreachable :: InfoTable -> InfoTable
filterUnreachable tab =
  over infoFunctions (HashMap.filterWithKey (const . isReachable graph)) tab
  where
    graph = createCallGraph tab
