module Juvix.Compiler.Core.Extra.Info where

import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Recursors
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language

mapInfo :: (Info -> Info) -> Node -> Node
mapInfo f = umap (modifyInfo f)

removeInfo :: IsInfo i => Key i -> Node -> Node
removeInfo k = mapInfo (Info.delete k)

lookupLocation :: Node -> Maybe Location
lookupLocation node =
  case Info.lookup kLocationInfo (getInfo node) of
    Just li -> Just (li ^. infoLocation)
    Nothing -> case Info.lookup kNameInfo (getInfo node) of
      Just ni -> Just $ ni ^. (infoName . nameLoc)
      Nothing -> Nothing
