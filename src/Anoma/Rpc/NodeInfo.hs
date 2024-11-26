module Anoma.Rpc.NodeInfo where

import Juvix.Prelude.Aeson
import Juvix.Prelude.Base

newtype NodeInfo = NodeInfo
  { _nodeInfoId :: Text
  }

$( deriveJSON
     defaultOptions
       { unwrapUnaryRecords = True,
         fieldLabelModifier = \case
           "_nodeInfoId" -> "node_id"
           _ -> impossibleError "All fields must be covered"
       }
     ''NodeInfo
 )

makeLenses ''NodeInfo
