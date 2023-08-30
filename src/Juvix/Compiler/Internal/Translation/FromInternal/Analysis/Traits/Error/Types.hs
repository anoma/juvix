module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Error.Types where

import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Pretty (fromGenericOptions)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Traits.Error.Pretty
import Juvix.Data.PPOutput
import Juvix.Prelude

newtype NotATrait = NotATrait
  { _notATraitExpression :: Expression
  }

makeLenses ''NotATrait

instance ToGenericError NotATrait where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (e ^. notATraitExpression)
          msg =
            "Expected a trait: "
              <+> ppCode opts' (e ^. notATraitExpression)

newtype NoInstance = NoInstance
  { _noInstanceType :: Expression
  }

makeLenses ''NoInstance

instance ToGenericError NoInstance where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (e ^. noInstanceType)
          msg =
            "No trait instance found for: "
              <+> ppCode opts' (e ^. noInstanceType)

data AmbiguousInstances = AmbiguousInstances
  { _ambiguousInstancesType :: Expression,
    _ambiguousInstancesInfos :: [InstanceInfo]
  }

makeLenses ''AmbiguousInstances

instance ToGenericError AmbiguousInstances where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (e ^. ambiguousInstancesType)
          locs = vsep $ map (pretty . getLoc . (^. instanceInfoResult)) (e ^. ambiguousInstancesInfos)
          msg =
            "Multiple trait instances found for "
              <+> ppCode opts' (e ^. ambiguousInstancesType)
                <> line
                <> "Matching instances found at: "
                <> indent' (align locs)

newtype ExplicitInstanceArgument = ExplicitInstanceArgument
  { _explicitInstanceArgumentExpression :: Expression
  }

makeLenses ''ExplicitInstanceArgument

instance ToGenericError ExplicitInstanceArgument where
  genericError e = generr
    where
      generr =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput "Explicit instance arguments not supported",
              _genericErrorIntervals = [i]
            }
        where
          i = getLoc (e ^. explicitInstanceArgumentExpression)
