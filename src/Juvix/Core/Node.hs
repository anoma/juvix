module Juvix.Core.Node where

{-
  This file defines the graph representation of JuvixCore (Node datatype) and
  general recursors on it.
-}

import Data.Functor.Identity
import Data.HashSet qualified as HashSet
import Juvix.Core.Info qualified as Info
import Juvix.Core.Info.BindingInfo
import Juvix.Core.Prelude

{---------------------------------------------------------------------------------}
{- Program graph datatype -}

-- Consecutive symbol IDs for reachable user functions.
type Symbol = Word

-- Tag of a constructor, uniquely identifying it. Tag values are consecutive and
-- separate from symbol IDs. We might need fixed special tags in Core for common
-- "builtin" constructors, e.g., unit, nat, lists, pairs, so that the code
-- generator can treat them specially.
data Tag = BuiltinTag BuiltinDataTag | UserTag Word
  deriving stock (Eq)

-- de Bruijn index
type Index = Int

-- `Node` is the type of nodes in the program graph. The nodes themselves
-- contain only runtime-relevant information. Runtime-irrelevant annotations
-- (including all type information) are stored in the infos associated with each
-- each.
data Node
  = -- De Bruijn index of a locally lambda-bound variable.
    Var !Info !Index
  | -- Global identifier of a function (with corresponding `GNode` in the global
    -- context).
    Ident !Info !Symbol
  | -- A builtin with no corresponding GNode, treated specially by the evaluator
    -- and the code generator. For example, basic arithmetic operations go into
    -- `Builtin`.
    Builtin !Info !BuiltinOp
  | -- A data constructor (the function that creates the data).
    Constructor !Info !Tag
  | ConstValue !Info {-# UNPACK #-} !Constant
  | -- A hole. It's a unit for the purposes of evaluation.
    Hole !Info
  | -- An axiom. Computationally a unit.
    Axiom !Info
  | App !Info !Node !Node
  | Lambda !Info !Node
  | -- `let x := value in body` is not reducible to lambda + application for the purposes
    -- of ML-polymorphic / dependent type checking or code generation!
    LetIn !Info !Node !Node
  | -- One-level case matching on the tag of a data constructor: `Case value
    -- branches`. `Case` is lazy: only the selected branch is evaluated. Lazy `if`
    -- can be implemented by a case on a boolean.
    Case !Info !Node ![CaseBranch]
  | -- Evaluation only: evaluated data constructor (the actual data).
    Data !Info !Tag ![Node]
  | -- Evaluation only: `LambdaClosure env body`
    LambdaClosure !Info !Env !Node
  | -- Evaluation only: a suspended term value which cannot be evaluated further,
    -- e.g., a hole applied to some arguments.
    Suspended !Info !Node

-- Other things we might need in the future:
-- - laziness annotations (converting these to closures/thunks should be done
--   further down the pipeline)
-- - primitive record projections (efficiency of evaluation / generated code)
-- - Fix and CoFix (anonymous recursion / co-recursion)
-- - with dependent types, it might actually be more reasonable to have Pi as
--   another node (because it's a binder); computationally it would be a unit,
--   erased in further stages of the pipeline
-- - with Pi a node, other basic type constructors should also be nodes:
--   TypeIdent (named type identifier available in the global context, e.g.,
--   inductive type), Universe

data Constant
  = ConstInteger !Integer
  | ConstBool !Bool

-- Other things we might need in the future:
-- - ConstFloat
-- - ConstString

data CaseBranch = CaseBranch !Tag !Node

-- all nodes in an environment must be closed (no free variables, i.e., de
-- Bruijn indices pointing outside the term)
type Env = [Node]

{---------------------------------------------------------------------------------}
{- Info -}

getInfo :: Node -> Info
getInfo = \case
  Var i _ -> i
  Ident i _ -> i
  Builtin i _ -> i
  Constructor i _ -> i
  ConstValue i _ -> i
  Hole i -> i
  Axiom i -> i
  App i _ _ -> i
  Lambda i _ -> i
  LetIn i _ _ -> i
  Case i _ _ -> i
  Data i _ _ -> i
  LambdaClosure i _ _ -> i
  Suspended i _ -> i

modifyInfoM :: Applicative m => (Info -> m Info) -> Node -> m Node
modifyInfoM f = \case
  Var i idx -> Var <$> f i <*> pure idx
  Ident i sym -> Ident <$> f i <*> pure sym
  Builtin i op -> Builtin <$> f i <*> pure op
  Constructor i tag -> Constructor <$> f i <*> pure tag
  ConstValue i v -> ConstValue <$> f i <*> pure v
  Hole i -> Hole <$> f i
  Axiom i -> Axiom <$> f i
  App i l r -> App <$> f i <*> pure l <*> pure r
  Lambda i b -> Lambda <$> f i <*> pure b
  LetIn i v b -> LetIn <$> f i <*> pure v <*> pure b
  Case i v bs -> Case <$> f i <*> pure v <*> pure bs
  Data i tag args -> Data <$> f i <*> pure tag <*> pure args
  LambdaClosure i env b -> LambdaClosure <$> f i <*> pure env <*> pure b
  Suspended i t -> Suspended <$> f i <*> pure t

modifyInfo :: (Info -> Info) -> Node -> Node
modifyInfo f n = runIdentity $ modifyInfoM (pure . f) n

{---------------------------------------------------------------------------------}
{- simple helper functions -}

mkApp :: Node -> [(Info, Node)] -> Node
mkApp = foldl' (\acc (i, n) -> App i acc n)

mkApp' :: Node -> [Node] -> Node
mkApp' = foldl' (App Info.empty)

unfoldApp :: Node -> (Node, [(Info, Node)])
unfoldApp = go []
  where
    go :: [(Info, Node)] -> Node -> (Node, [(Info, Node)])
    go acc n = case n of
      App i l r -> go ((i, r) : acc) l
      _ -> (n, acc)

children :: Node -> [Node]
children = \case
  App _ l r -> [l, r]
  Lambda _ b -> [b]
  LetIn _ v b -> [v, b]
  Case _ v bs -> v : map (\(CaseBranch _ br) -> br) bs
  Data _ _ args -> args
  LambdaClosure _ env b -> b : env
  Suspended _ t -> [t]
  _ -> []

-- children not under binder
schildren :: Node -> [Node]
schildren = \case
  App _ l r -> [l, r]
  LetIn _ v _ -> [v]
  Case _ v bs -> v : map (\(CaseBranch _ br) -> br) bs
  Data _ _ args -> args
  LambdaClosure _ env _ -> env
  Suspended _ t -> [t]
  _ -> []

-- children under binder
bchildren :: Node -> [Node]
bchildren = \case
  Lambda _ b -> [b]
  LetIn _ _ b -> [b]
  LambdaClosure _ _ b -> [b]
  _ -> []

{---------------------------------------------------------------------------------}
{- General recursors on Node  -}

-- Note: In the (distant) future, with dependent types, the type information
-- will contain Nodes. Then mapping/folding needs to be performed also on the
-- Nodes stored as type information. This will require modifying type
-- information in the recursors below.

-- a collector collects information top-down on a single path in the program
-- tree
data Collector a c = Collector
  { _cEmpty :: c,
    _cCollect :: a -> c -> c
  }

makeLenses ''Collector

unitCollector :: Collector a ()
unitCollector = Collector () (\_ _ -> ())

bindingCollector :: Collector Info c -> Collector Node c
bindingCollector coll = Collector (coll ^. cEmpty) collect
  where
    collect n c = case n of
      Lambda i _ -> (coll ^. cCollect) i c
      LetIn i _ _ -> (coll ^. cCollect) i c
      LambdaClosure i _ _ -> (coll ^. cCollect) i c
      _ -> c

bindingInfoCollector :: Collector Node [Maybe BindingInfo]
bindingInfoCollector =
  bindingCollector
    ( Collector
        []
        (\i c -> Info.lookup kBindingInfo i : c)
    )

bindingNumCollector :: Collector Node Index
bindingNumCollector = bindingCollector (Collector 0 (const (+ 1)))

-- `umapG` maps the nodes bottom-up, i.e., when invoking the mapper function the
-- recursive subnodes have already been mapped
umapG ::
  forall c m.
  Monad m =>
  Collector Node c ->
  (c -> Node -> m Node) ->
  Node ->
  m Node
umapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m Node
    go c n = case n of
      App i l r -> f c =<< (App i <$> go c' l <*> go c' r)
      Lambda i body -> f c . Lambda i =<< go c' body
      LetIn i value body -> f c =<< (LetIn i <$> go c' value <*> go c' body)
      Case i value bs -> f c =<< (Case i <$> go c' value <*> mapM (\(CaseBranch tag br) -> CaseBranch tag <$> go c' br) bs)
      Data i tag args -> f c . Data i tag =<< mapM (go c') args
      LambdaClosure i env body -> f c =<< (LambdaClosure i <$> mapM (go c') env <*> go c' body)
      Suspended i t -> f c . Suspended i =<< go c' t
      _ -> f c n
      where
        c' = (coll ^. cCollect) n c

umapM :: Monad m => (Node -> m Node) -> Node -> m Node
umapM f = umapG unitCollector (const f)

umapMB :: Monad m => ([Maybe BindingInfo] -> Node -> m Node) -> Node -> m Node
umapMB f = umapG bindingInfoCollector f

umapMN :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
umapMN f = umapG bindingNumCollector f

umap :: (Node -> Node) -> Node -> Node
umap f n = runIdentity $ umapM (return . f) n

umapB :: ([Maybe BindingInfo] -> Node -> Node) -> Node -> Node
umapB f n = runIdentity $ umapMB (\is -> return . f is) n

umapN :: (Index -> Node -> Node) -> Node -> Node
umapN f n = runIdentity $ umapMN (\idx -> return . f idx) n

-- `dmapG` maps the nodes top-down
dmapG ::
  forall c m.
  Monad m =>
  Collector Node c ->
  ( c ->
    Node ->
    m Node
  ) ->
  Node ->
  m Node
dmapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m Node
    go c n = do
      n' <- f c n
      let c' = (coll ^. cCollect) n' c
      case n' of
        App i l r -> App i <$> go c' l <*> go c' r
        Lambda i body -> Lambda i <$> go c' body
        LetIn i value body -> LetIn i <$> go c' value <*> go c' body
        Case i value bs -> Case i <$> go c' value <*> mapM (\(CaseBranch tag br) -> CaseBranch tag <$> go c' br) bs
        Data i tag args -> Data i tag <$> mapM (go c') args
        LambdaClosure i env body -> LambdaClosure i <$> mapM (go c') env <*> go c' body
        Suspended i t -> Suspended i <$> go c' t
        _ -> return n'

dmapM :: Monad m => (Node -> m Node) -> Node -> m Node
dmapM f = dmapG unitCollector (const f)

dmapMB :: Monad m => ([Maybe BindingInfo] -> Node -> m Node) -> Node -> m Node
dmapMB f = dmapG bindingInfoCollector f

dmapMN :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
dmapMN f = dmapG bindingNumCollector f

dmap :: (Node -> Node) -> Node -> Node
dmap f n = runIdentity $ dmapM (return . f) n

dmapB :: ([Maybe BindingInfo] -> Node -> Node) -> Node -> Node
dmapB f n = runIdentity $ dmapMB (\is -> return . f is) n

dmapN :: (Index -> Node -> Node) -> Node -> Node
dmapN f n = runIdentity $ dmapMN (\idx -> return . f idx) n

-- `ufoldG` folds the tree bottom-up; `uplus` combines the values - it should be
-- commutative and associative
ufoldG ::
  forall c a m.
  Monad m =>
  Collector Node c ->
  (a -> a -> a) ->
  (c -> Node -> m a) ->
  Node ->
  m a
ufoldG coll uplus f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m a
    go c n = case n of
      App _ l r -> uplus <$> f c n <*> (uplus <$> go c' l <*> go c' r)
      Lambda _ body -> uplus <$> f c n <*> go c' body
      LetIn _ value body -> uplus <$> f c n <*> (uplus <$> go c' value <*> go c' body)
      Case _ value bs -> uplus <$> f c n <*> foldr (liftM2 uplus . (\(CaseBranch _ br) -> go c' br)) (go c' value) bs
      Data _ _ args -> foldr (liftM2 uplus . go c') (f c n) args
      LambdaClosure _ env body -> uplus <$> f c n <*> foldr (liftM2 uplus . go c') (go c' body) env
      Suspended _ t -> uplus <$> f c n <*> go c' t
      _ -> f c n
      where
        c' = (coll ^. cCollect) n c

ufoldM :: Monad m => (a -> a -> a) -> (Node -> m a) -> Node -> m a
ufoldM uplus f = ufoldG unitCollector uplus (const f)

ufoldMB :: Monad m => (a -> a -> a) -> ([Maybe BindingInfo] -> Node -> m a) -> Node -> m a
ufoldMB uplus f = ufoldG bindingInfoCollector uplus f

ufoldMN :: Monad m => (a -> a -> a) -> (Index -> Node -> m a) -> Node -> m a
ufoldMN uplus f = ufoldG bindingNumCollector uplus f

ufold :: (a -> a -> a) -> (Node -> a) -> Node -> a
ufold uplus f n = runIdentity $ ufoldM uplus (return . f) n

ufoldB :: (a -> a -> a) -> ([Maybe BindingInfo] -> Node -> a) -> Node -> a
ufoldB uplus f n = runIdentity $ ufoldMB uplus (\is -> return . f is) n

ufoldN :: (a -> a -> a) -> (Index -> Node -> a) -> Node -> a
ufoldN uplus f n = runIdentity $ ufoldMN uplus (\idx -> return . f idx) n

walk :: Monad m => (Node -> m ()) -> Node -> m ()
walk = ufoldM mappend

walkB :: Monad m => ([Maybe BindingInfo] -> Node -> m ()) -> Node -> m ()
walkB = ufoldMB mappend

walkN :: Monad m => (Index -> Node -> m ()) -> Node -> m ()
walkN = ufoldMN mappend

gather :: (a -> Node -> a) -> a -> Node -> a
gather f acc n = fst $ run $ runState acc (walk (\n' -> modify (`f` n')) n)

gatherB :: ([Maybe BindingInfo] -> a -> Node -> a) -> a -> Node -> a
gatherB f acc n = fst $ run $ runState acc (walkB (\is n' -> modify (\a -> f is a n')) n)

gatherN :: (Index -> a -> Node -> a) -> a -> Node -> a
gatherN f acc n = fst $ run $ runState acc (walkN (\idx n' -> modify (\a -> f idx a n')) n)

{---------------------------------------------------------------------------}
{- useful functions implemented using general recursors -}

isClosed :: Node -> Bool
isClosed = ufoldN (&&) go
  where
    go :: Index -> Node -> Bool
    go k = \case
      Var _ idx | idx >= k -> False
      _ -> True

getFreeVars :: Node -> HashSet Index
getFreeVars = gatherN go HashSet.empty
  where
    go :: Index -> HashSet Index -> Node -> HashSet Index
    go k acc = \case
      Var _ idx | idx >= k -> HashSet.insert (idx - k) acc
      _ -> acc

getIdents :: Node -> HashSet Symbol
getIdents = gather go HashSet.empty
  where
    go :: HashSet Symbol -> Node -> HashSet Symbol
    go acc = \case
      Ident _ sym -> HashSet.insert sym acc
      _ -> acc

countFreeVarOccurrences :: Index -> Node -> Int
countFreeVarOccurrences idx = gatherN go 0
  where
    go k acc = \case
      Var _ idx' | idx' == idx + k -> acc + 1
      _ -> acc

-- increase all free variable indices by a given value
shift :: Index -> Node -> Node
shift m = umapN go
  where
    go k n = case n of
      Var i idx | idx >= k -> Var i (idx + m)
      _ -> n

-- substitute a term t for the free variable with de Bruijn index 0, avoiding
-- variable capture
subst :: Node -> Node -> Node
subst t = umapN go
  where
    go k n = case n of
      Var _ idx | idx == k -> shift k t
      _ -> n

-- reduce all beta redexes present in a term and the ones created upwards
-- (i.e., a "beta-development")
reduceBeta :: Node -> Node
reduceBeta = umap go
  where
    go :: Node -> Node
    go n = case n of
      App _ (Lambda _ body) arg -> subst arg body
      _ -> n

-- substitution of all free variables for values in a closed environment
substEnv :: Env -> Node -> Node
substEnv env = umapN go
  where
    go k n = case n of
      Var _ idx | idx >= k -> env !! k
      _ -> n

removeClosures :: Node -> Node
removeClosures = umap go
  where
    go :: Node -> Node
    go n = case n of
      LambdaClosure i env b -> substEnv env (Lambda i b)
      _ -> n
