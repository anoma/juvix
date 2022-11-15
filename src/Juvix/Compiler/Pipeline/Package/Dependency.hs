module Juvix.Compiler.Pipeline.Package.Dependency where

import Data.Aeson.TH
import Juvix.Prelude
import Lens.Micro.Platform qualified as Lens

-- | dependencies paths are canonicalized just after reading the package
newtype Dependency = Dependency
  { _dependencyPath :: FilePath
  }
  deriving stock (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = over Lens._head toLower . dropPrefix "_dependency",
         rejectUnknownFields = True,
         unwrapUnaryRecords = True
       }
     ''Dependency
 )

makeLenses ''Dependency
