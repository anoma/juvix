module Juvix.Compiler.Tree.Transformation.Base
  ( module Juvix.Compiler.Tree.Transformation.Base,
    module Juvix.Compiler.Tree.Data.InfoTable,
    module Juvix.Compiler.Tree.Language,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Data.InfoTableBuilder
import Juvix.Compiler.Tree.Language

mapFunctionsM :: (Monad m) => (FunctionInfo -> m FunctionInfo) -> InfoTable -> m InfoTable
mapFunctionsM = overM infoFunctions . mapM

mapInductivesM :: (Monad m) => (InductiveInfo -> m InductiveInfo) -> InfoTable -> m InfoTable
mapInductivesM = overM infoInductives . mapM

mapConstructorsM :: (Monad m) => (ConstructorInfo -> m ConstructorInfo) -> InfoTable -> m InfoTable
mapConstructorsM = overM infoConstrs . mapM

mapFunctions :: (FunctionInfo -> FunctionInfo) -> InfoTable -> InfoTable
mapFunctions = over infoFunctions . fmap

mapInductives :: (InductiveInfo -> InductiveInfo) -> InfoTable -> InfoTable
mapInductives = over infoInductives . fmap

mapConstructors :: (ConstructorInfo -> ConstructorInfo) -> InfoTable -> InfoTable
mapConstructors = over infoConstrs . fmap

mapT :: (Symbol -> Node -> Node) -> InfoTable -> InfoTable
mapT f = over infoFunctions (HashMap.mapWithKey (over functionCode . f))

mapT' :: (Symbol -> Node -> Sem (InfoTableBuilder ': r) Node) -> InfoTable -> Sem r InfoTable
mapT' f tab =
  fmap fst $
    runInfoTableBuilder $
      mapM_
        (\(sym, fi) -> overM functionCode (f sym) fi >>= registerFunction)
        (HashMap.toList (tab ^. infoFunctions))

walkT :: (Applicative f) => (Symbol -> Node -> f ()) -> InfoTable -> f ()
walkT f tab = for_ (HashMap.toList (tab ^. infoFunctions)) (\(k, v) -> f k (v ^. functionCode))
