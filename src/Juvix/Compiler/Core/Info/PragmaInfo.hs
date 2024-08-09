module Juvix.Compiler.Core.Info.PragmaInfo where

import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language

newtype PragmasInfo = PragmasInfo
  { _infoPragmas :: [Pragmas]
  }

instance IsInfo PragmasInfo

kPragmasInfo :: Key PragmasInfo
kPragmasInfo = Proxy

makeLenses ''PragmasInfo

newtype PragmaInfo = PragmaInfo
  { _infoPragma :: Pragmas
  }

instance IsInfo PragmaInfo

kPragmaInfo :: Key PragmaInfo
kPragmaInfo = Proxy

makeLenses ''PragmaInfo

getInfoPragmas :: Info -> [Pragmas]
getInfoPragmas i =
  case Info.lookup kPragmasInfo i of
    Just PragmasInfo {..} -> _infoPragmas
    Nothing -> []

setInfoPragmas :: [Pragmas] -> Info -> Info
setInfoPragmas = Info.insert . PragmasInfo

overInfoPragmas :: (Pragmas -> Pragmas) -> Info -> Info
overInfoPragmas f i = case Info.lookup kPragmasInfo i of
  Just PragmasInfo {..} -> setInfoPragmas (map f _infoPragmas) i
  Nothing -> i

getInfoPragma :: Info -> Pragmas
getInfoPragma i =
  case Info.lookup kPragmaInfo i of
    Just PragmaInfo {..} -> _infoPragma
    Nothing -> mempty

setInfoPragma :: Pragmas -> Info -> Info
setInfoPragma = Info.insert . PragmaInfo

overInfoPragma :: (Pragmas -> Pragmas) -> Info -> Info
overInfoPragma f i = case Info.lookup kPragmaInfo i of
  Just PragmaInfo {..} -> setInfoPragma (f _infoPragma) i
  Nothing -> i

getNodePragmas :: Node -> Pragmas
getNodePragmas = getInfoPragma . getInfo

setNodePragmas :: Pragmas -> Node -> Node
setNodePragmas = modifyInfo . setInfoPragma
