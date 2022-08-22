module Juvix.Compiler.Core.Transformation.Base
  ( module Juvix.Compiler.Core.Transformation.Base,
    module Juvix.Compiler.Core.Data.InfoTable,
    module Juvix.Compiler.Core.Language,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Language

type Transformation = InfoTable -> InfoTable

mapT :: (Node -> Node) -> InfoTable -> InfoTable
mapT f tab = tab {_identContext = HashMap.map f (tab ^. identContext)}

mapT' :: (Node -> Sem (InfoTableBuilder ': r) Node) -> InfoTable -> Sem r InfoTable
mapT' f tab = fmap fst $ runInfoTableBuilder tab $ do
  mapM_
    (\(k, v) -> f v >>= registerIdentNode k)
    (HashMap.toList (tab ^. identContext))
