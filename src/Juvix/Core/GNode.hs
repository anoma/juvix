module Juvix.Core.GNode where

{-
  This file defines the graph representation of JuvixCore (GNode datatype) and
  general recursors on it.
-}

import Data.Functor.Identity
import Juvix.Prelude

{---------------------------------------------------------------------------------}
{- Program graph datatype -}

-- Consecutive symbol IDs for builtins and reachable user functions.
type Symbol = Word

-- Tag of a constructor, uniquely identifying it. Tag values are consecutive and
-- separate from symbol IDs. We might need fixed special tag values in Core for
-- common "builtin" constructors, e.g., lists, pairs, so that the code generator
-- can treat them specially.
type Tag = Word

-- de Bruijn index
type Index = Int

-- `GNode i` is the type of nodes in the program graph, where `i` is the info
-- type. `GNode` itself contains only runtime-relevant information.
-- Runtime-irrelevant annotations (including all type information) are stored in
-- the `i` argument of the `NodeInfo` nodes.
data GNode i
  = -- De Bruijn index of a locally lambda-bound variable.
    Var !i !Index
  | -- Global identifier of a function (with corresponding `GNode` in the global
    -- context).
    Ident !i !Symbol
  | -- Global identifier of an external / builtin (no corresponding GNode). For
    -- example, basic arithmetic operations go into `Builtin`. The numeric
    -- symbol values for basic builtin operations (e.g. arithmetic) should be
    -- fixed in Core, so that the evaluator and the code generator know about
    -- them and can treat them specially.
    Builtin !i !Symbol
  | Literal !i {-# UNPACK #-} !Constant
  | App !i !(GNode i) !(GNode i)
  | Lambda !i !(GNode i)
  | -- `let x := value in body` is not reducible to lambda + application for the purposes
    -- of ML-polymorphic / dependent type checking or code generation!
    LetIn !i !(GNode i) !(GNode i)
  | -- Data constructor.
    Data !i !Tag ![GNode i]
  | -- One-level case matching on the tag of a data constructor: `Case value
    -- branches`. `Case` is lazy: only the selected branch is evaluated. Lazy `if`
    -- can be implemented by a case on a boolean.
    Case !i !(GNode i) ![CaseBranch i]
  | -- Execution only: `LambdaClosure env body`
    LambdaClosure !i ![GNode i] !(GNode i)

-- Other things we might need in the future:
-- - laziness annotations (converting these to closures should be done further
--   down the pipeline)
-- - primitive record projections (efficiency of evaluation / code generation)
-- - Fix and CoFix (anonymous recursion / co-recursion)

data Constant
  = ConstInteger !Integer
  | ConstBool !Bool
  | -- A hole. It's a unit for the purposes of evaluation.
    ConstHole

-- Other things we might need in the future:
-- - ConstFloat
-- - ConstString
-- - ConstType: computationally a unit, corresponds to a type argument; the
--   attached an info stores the type information; erased further down the
--   pipeline

data CaseBranch i = CaseBranch !Tag !(GNode i)

{---------------------------------------------------------------------------------}
{- General recursors on GNode  -}

-- a collector collects information top-down on a single path in the program
-- tree
data Collector a c = Collector
  {
    _cEmpty :: c,
    _cCollect :: a -> c -> c
  }

makeLenses ''Collector

unitCollector :: Collector a ()
unitCollector = Collector () (\_ _ -> ())

bindingCollector :: Collector i c -> Collector (GNode i) c
bindingCollector coll = Collector (coll ^. cEmpty) collect
  where
    collect n c = case n of
      Lambda i _ -> (coll ^. cCollect) i c
      LetIn i _ _ -> (coll ^. cCollect) i c
      LambdaClosure i _ _ -> (coll ^. cCollect) i c
      _ -> c

-- `umapG` maps the nodes bottom-up, i.e., when invoking the mapper function the
-- recursive subnodes have already been mapped
umapG :: Monad m => Collector (GNode i) c -> (c -> GNode i -> m (GNode i)) -> GNode i -> m (GNode i)
umapG coll f = go (coll ^. cEmpty)
  where
    go c n = case n of
      App i l r -> f c =<< (App i <$> go c' l <*> go c' r)
      Lambda i body -> f c . Lambda i =<< go c' body
      LetIn i value body -> f c =<< (LetIn i <$> go c' value <*> go c' body)
      Data i tag args -> f c . Data i tag =<< mapM (go c') args
      Case i value bs -> f c =<< (Case i <$> go c' value <*> mapM (\(CaseBranch tag br) -> CaseBranch tag <$> go c' br) bs)
      LambdaClosure i env body -> f c =<< (LambdaClosure i <$> mapM (go c') env <*> go c' body)
      _ -> f c n
      where
        c' = (coll ^. cCollect) n c

umapM :: Monad m => (GNode i -> m (GNode i)) -> GNode i -> m (GNode i)
umapM f = umapG unitCollector (const f)

umapMB :: Monad m => ([i] -> GNode i -> m (GNode i)) -> GNode i -> m (GNode i)
umapMB f = umapG (bindingCollector (Collector [] (:))) f

umapMN :: Monad m => (Index -> GNode i -> m (GNode i)) -> GNode i -> m (GNode i)
umapMN f = umapG (bindingCollector (Collector 0 (const (+ 1)))) f

umap :: (GNode i -> GNode i) -> GNode i -> GNode i
umap f n = runIdentity $ umapM (return . f) n

umapB :: ([i] -> GNode i -> GNode i) -> GNode i -> GNode i
umapB f n = runIdentity $ umapMB (\is -> return . f is) n

umapN :: (Index -> GNode i -> GNode i) -> GNode i -> GNode i
umapN f n = runIdentity $ umapMN (\idx -> return . f idx) n

-- `dmapG` maps the nodes top-down
dmapG :: Monad m => Collector (GNode i) c -> (c -> GNode i -> m (GNode i)) -> GNode i -> m (GNode i)
dmapG coll f = go (coll ^. cEmpty)
  where
    go c n = do
      n' <- f c n
      let c' = (coll ^. cCollect) n' c
      case n' of
        App i l r -> App i <$> go c' l <*> go c' r
        Lambda i body -> Lambda i <$> go c' body
        LetIn i value body -> LetIn i <$> go c' value <*> go c' body
        Data i tag args -> Data i tag <$> mapM (go c') args
        Case i value bs -> Case i <$> go c' value <*> mapM (\(CaseBranch tag br) -> CaseBranch tag <$> go c' br) bs
        LambdaClosure i env body -> LambdaClosure i <$> mapM (go c') env <*> go c' body
        _ -> return n'

dmapM :: Monad m => (GNode i -> m (GNode i)) -> GNode i -> m (GNode i)
dmapM f = dmapG unitCollector (const f)

dmapMB :: Monad m => ([i] -> GNode i -> m (GNode i)) -> GNode i -> m (GNode i)
dmapMB f = dmapG (bindingCollector (Collector [] (:))) f

dmapMN :: Monad m => (Index -> GNode i -> m (GNode i)) -> GNode i -> m (GNode i)
dmapMN f = dmapG (bindingCollector (Collector 0 (const (+ 1)))) f

dmap :: (GNode i -> GNode i) -> GNode i -> GNode i
dmap f n = runIdentity $ dmapM (return . f) n

dmapB :: ([i] -> GNode i -> GNode i) -> GNode i -> GNode i
dmapB f n = runIdentity $ dmapMB (\is -> return . f is) n

dmapN :: (Index -> GNode i -> GNode i) -> GNode i -> GNode i
dmapN f n = runIdentity $ dmapMN (\idx -> return . f idx) n

-- `ufoldG` folds the tree bottom-up; `uplus` combines the values - it should be
-- commutative and associative
ufoldG :: Monad m => Collector (GNode i) c -> (a -> a -> a) -> (c -> GNode i -> m a) -> GNode i -> m a
ufoldG coll uplus f = go (coll ^. cEmpty)
  where
    go c n = case n of
      App _ l r -> uplus <$> f c n <*> (uplus <$> go c' l <*> go c' r)
      Lambda _ body -> uplus <$> f c n <*> go c' body
      LetIn _ value body -> uplus <$> f c n <*> (uplus <$> go c' value <*> go c' body)
      Data _ _ args -> foldr (liftM2 uplus . go c') (f c n) args
      Case _ value bs -> uplus <$> f c n <*> foldr (liftM2 uplus . (\(CaseBranch _ br) -> go c' br)) (go c' value) bs
      LambdaClosure _ env body -> uplus <$> f c n <*> foldr (liftM2 uplus . go c') (go c' body) env
      _ -> f c n
      where
        c' = (coll ^. cCollect) n c
