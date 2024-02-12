module Juvix.Compiler.Tree.Transformation.Generic.Base where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Tree.Data.InfoTable.Base
import Juvix.Compiler.Tree.Data.InfoTableBuilder.Base
import Juvix.Compiler.Tree.Language.Base

mapFunctionsM :: (Monad m) => (FunctionInfo' a e -> m (FunctionInfo' a e)) -> InfoTable' a e -> m (InfoTable' a e)
mapFunctionsM = overM infoFunctions . mapM

mapInductivesM :: (Monad m) => (InductiveInfo -> m InductiveInfo) -> InfoTable' a e -> m (InfoTable' a e)
mapInductivesM = overM infoInductives . mapM

mapConstructorsM :: (Monad m) => (ConstructorInfo -> m ConstructorInfo) -> InfoTable' a e -> m (InfoTable' a e)
mapConstructorsM = overM infoConstrs . mapM

mapFunctions :: (FunctionInfo' a e -> FunctionInfo' a e) -> InfoTable' a e -> InfoTable' a e
mapFunctions = over infoFunctions . fmap

mapInductives :: (InductiveInfo -> InductiveInfo) -> InfoTable' a e -> InfoTable' a e
mapInductives = over infoInductives . fmap

mapConstructors :: (ConstructorInfo -> ConstructorInfo) -> InfoTable' a e -> InfoTable' a e
mapConstructors = over infoConstrs . fmap

mapT :: (Symbol -> a -> a) -> InfoTable' a e -> InfoTable' a e
mapT f = over infoFunctions (HashMap.mapWithKey (over functionCode . f))

mapT' :: forall a e r. (Symbol -> a -> Sem (InfoTableBuilder' a e ': r) a) -> InfoTable' a e -> Sem r (InfoTable' a e)
mapT' f tab =
  fmap fst $
    runInfoTableBuilderWithInfoTable tab $
      mapM_
        (\(sym, fi) -> overM functionCode (f sym) fi >>= registerFunction' @a @e)
        (HashMap.toList (tab ^. infoFunctions))

walkT :: (Applicative f) => (Symbol -> a -> f ()) -> InfoTable' a e -> f ()
walkT f tab = for_ (HashMap.toList (tab ^. infoFunctions)) (\(k, v) -> f k (v ^. functionCode))
