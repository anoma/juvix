module Juvix.Compiler.Core.Transformation.ComputeTypeInfo(computeTypeInfo) where

import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.TypeInfo qualified as Info

-- TODO: store the type of each node in its info
computeNodeTypeInfo :: InfoTable -> Node -> Node
computeNodeTypeInfo _ = umap (Info.setNodeType mkDynamic')

computeTypeInfo :: InfoTable -> InfoTable
computeTypeInfo tab = mapT (const (computeNodeTypeInfo tab)) tab
