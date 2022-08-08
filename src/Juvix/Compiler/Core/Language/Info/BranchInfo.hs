module Juvix.Compiler.Core.Language.Info.BranchInfo where

import Juvix.Compiler.Core.Language.Base

newtype BranchInfo = BranchInfo
  { _infoName :: Name
  }

instance IsInfo BranchInfo

kBranchInfo :: Key BranchInfo
kBranchInfo = Proxy

newtype CaseBranchInfo = CaseBranchInfo
  { _infoBranches :: [BranchInfo]
  }

instance IsInfo CaseBranchInfo

kCaseBranchInfo :: Key CaseBranchInfo
kCaseBranchInfo = Proxy

makeLenses ''BranchInfo
makeLenses ''CaseBranchInfo
