module Juvix.Compiler.Tree.Extra.Info where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Backend
import Juvix.Compiler.Tree.Data.InfoTable.Base
import Juvix.Compiler.Tree.Language.Base

userConstrs :: InfoTable' a e -> [ConstructorInfo]
userConstrs tab =
  filter (\ci -> not (isBuiltinTag (ci ^. constructorTag))) $
    HashMap.elems (tab ^. infoConstrs)

computeUIDs :: Limits -> InfoTable' a e -> HashMap Tag Int
computeUIDs lims tab =
  HashMap.fromList $
    zipWith
      (\ci uid -> (ci ^. constructorTag, uid))
      (userConstrs tab)
      [lims ^. limitsBuiltinUIDsNum ..]

computeFUIDs :: InfoTable' a e -> HashMap Symbol Int
computeFUIDs tab =
  HashMap.fromList $
    zipWith
      (\fi fuid -> (fi ^. functionSymbol, fuid))
      (HashMap.elems (tab ^. infoFunctions))
      [0 ..]

computeCIDs :: InfoTable' a e -> HashMap Tag Int
computeCIDs tab = HashMap.fromList $ concatMap go (tab ^. infoInductives)
  where
    go :: InductiveInfo -> [(Tag, Int)]
    go InductiveInfo {..} = zip _inductiveConstructors [0 ..]
