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

mapIdents :: (IdentifierInfo -> IdentifierInfo) -> InfoTable -> InfoTable
mapIdents = over infoIdentifiers . fmap

mapInductives :: (InductiveInfo -> InductiveInfo) -> InfoTable -> InfoTable
mapInductives = over infoInductives . fmap

mapConstructors :: (ConstructorInfo -> ConstructorInfo) -> InfoTable -> InfoTable
mapConstructors = over infoConstructors . fmap

mapAxioms :: (AxiomInfo -> AxiomInfo) -> InfoTable -> InfoTable
mapAxioms = over infoAxioms . fmap

mapT :: (Symbol -> Node -> Node) -> InfoTable -> InfoTable
mapT f tab = tab {_identContext = HashMap.mapWithKey f (tab ^. identContext)}

mapT' :: (Symbol -> Node -> Sem (InfoTableBuilder ': r) Node) -> InfoTable -> Sem r InfoTable
mapT' f tab =
  fmap fst $
    runInfoTableBuilder tab $
      mapM_
        (\(k, v) -> f k v >>= registerIdentNode k)
        (HashMap.toList (tab ^. identContext))

walkT :: (Applicative f) => (Symbol -> Node -> f ()) -> InfoTable -> f ()
walkT f tab = for_ (HashMap.toList (tab ^. identContext)) (uncurry f)

mapAllNodes :: (Node -> Node) -> InfoTable -> InfoTable
mapAllNodes f tab =
  mapAxioms convertAxiom $
    mapInductives convertInductive $
      mapConstructors convertConstructor $
        mapIdents convertIdent $
          mapT (const f) tab
  where
    convertIdent :: IdentifierInfo -> IdentifierInfo
    convertIdent ii =
      ii
        { _identifierType = f (ii ^. identifierType),
          _identifierArgsInfo = map (over argumentType f) (ii ^. identifierArgsInfo)
        }

    convertConstructor :: ConstructorInfo -> ConstructorInfo
    convertConstructor = over constructorType f

    convertInductive :: InductiveInfo -> InductiveInfo
    convertInductive ii =
      ii
        { _inductiveKind = f (ii ^. inductiveKind),
          _inductiveParams = map (over paramKind f) (ii ^. inductiveParams),
          _inductiveConstructors = map convertConstructor (ii ^. inductiveConstructors)
        }

    convertAxiom :: AxiomInfo -> AxiomInfo
    convertAxiom = over axiomType f

