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

binderInfoCollector' :: BinderList Binder -> Collector (Int, [Binder]) (BinderList Binder)
binderInfoCollector' ini = Collector ini collect
  where
    collect :: (Int, [Binder]) -> BinderList Binder -> BinderList Binder
    collect (k, bi) c
      | k == 0 = c
      | otherwise = BL.prepend (reverse bi) c

binderInfoCollector :: Collector (Int, [Binder]) (BinderList Binder)
binderInfoCollector = binderInfoCollector' mempty

binderNumCollector' :: Int -> Collector (Int, [Binder]) Index
binderNumCollector' ini = Collector ini (\(k, _) c -> c + k)

binderNumCollector :: Collector (Int, [Binder]) Index
binderNumCollector = binderNumCollector' 0

pairCollector :: Collector a b -> Collector a c -> Collector a (b, c)
pairCollector coll1 coll2 =
  Collector
    { _cEmpty = (coll1 ^. cEmpty, coll2 ^. cEmpty),
      _cCollect = \a (b, c) -> ((coll1 ^. cCollect) a b, (coll2 ^. cCollect) a c)
    }

identityCollector :: c -> Collector a c
identityCollector ini = Collector ini (const id)
