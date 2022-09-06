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

binderInfoCollector' :: BinderList Info -> Collector (Int, [Info]) (BinderList Info)
binderInfoCollector' ini = Collector ini collect
  where
    collect :: (Int, [Info]) -> BinderList Info -> BinderList Info
    collect (k, bi) c
      | k == 0 = c
      | otherwise = BL.prepend (reverse bi) c

binderInfoCollector :: Collector (Int, [Info]) (BinderList Info)
binderInfoCollector = binderInfoCollector' mempty

binderNumCollector' :: Int -> Collector (Int, [Info]) Index
binderNumCollector' ini = Collector ini (\(k, _) c -> c + k)

binderNumCollector :: Collector (Int, [Info]) Index
binderNumCollector = binderNumCollector' 0
