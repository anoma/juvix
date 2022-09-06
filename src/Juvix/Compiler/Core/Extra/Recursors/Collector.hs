module Juvix.Compiler.Core.Extra.Recursors.Collector where

import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Language

-- | a collector collects information top-down on a single path in the program
-- tree
data Collector a c = Collector
  { _cEmpty :: c,
    _cCollect :: a -> c -> c
  }

makeLenses ''Collector

unitCollector :: Collector a ()
unitCollector = Collector () (\_ _ -> ())

binderInfoCollector :: Collector (Int, [Info]) (BinderList Info)
binderInfoCollector =
  Collector
    mempty
    (\(k, bi) c -> if k == 0 then c else BL.prepend (reverse bi) c)

binderNumCollector :: Collector (Int, [Info]) Index
binderNumCollector = Collector 0 (\(k, _) c -> c + k)
