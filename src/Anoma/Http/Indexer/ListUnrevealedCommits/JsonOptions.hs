-- | Options needed to derive JSON instances need to be put in a separate file due to
-- Template Haskell stage restriction
module Anoma.Http.Indexer.ListUnrevealedCommits.JsonOptions where

import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

responseOptions :: Aeson.Options
responseOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "_responseCommits" -> "commits"
        _ -> impossibleError "All fields must be covered"
    }
