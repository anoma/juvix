module Juvix.Compiler.Core.Info.BinderInfo where

import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language

-- | Info about a single binder. Associated with Lambda and Pi.
newtype BinderInfo = BinderInfo {_infoBinder :: Info}

instance IsInfo BinderInfo

kBinderInfo :: Key BinderInfo
kBinderInfo = Proxy

-- | Info about multiple binders. Associated with LetRec.
newtype BindersInfo = BindersInfo {_infoBinders :: [Info]}

instance IsInfo BindersInfo

kBindersInfo :: Key BindersInfo
kBindersInfo = Proxy

makeLenses ''BinderInfo
makeLenses ''BindersInfo

getInfoBinder :: Info -> Info
getInfoBinder i =
  case Info.lookup kBinderInfo i of
    Just (BinderInfo {..}) -> _infoBinder
    Nothing -> Info.empty

getInfoBinders :: Int -> Info -> [Info]
getInfoBinders n i =
  case Info.lookup kBindersInfo i of
    Just (BindersInfo {..}) -> _infoBinders
    Nothing -> replicate n Info.empty

setInfoBinders :: [Info] -> Info -> Info
setInfoBinders = Info.insert . BindersInfo

setInfoBinder :: Info -> Info -> Info
setInfoBinder = Info.insert . BinderInfo

singletonInfoBinder :: Info -> Info
singletonInfoBinder i = setInfoBinder i mempty
