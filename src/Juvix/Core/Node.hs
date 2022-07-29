module Juvix.Core.Node where

{-
  This file defines the tree representation of JuvixCore (Node datatype) and
  general recursors on it.
-}

import Data.Functor.Identity
import Data.HashSet qualified as HashSet
import Juvix.Core.Info qualified as Info
import Juvix.Core.Info.BinderInfo
import Juvix.Core.Prelude

{---------------------------------------------------------------------------------}
{- Program tree datatype -}

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

-- `Node` is the type of nodes in the program tree. The nodes themselves
-- contain only runtime-relevant information. Runtime-irrelevant annotations
-- (including all type information) are stored in the infos associated with each
-- node.
data Node
  = -- De Bruijn index of a locally lambda-bound variable.
    Var !Info !Index
  | -- Global identifier of a function (with corresponding `Node` in the global
    -- context).
    Ident !Info !Symbol
  | -- A builtin with no corresponding Node, treated specially by the evaluator
    -- and the code generator. For example, basic arithmetic operations go into
    -- `Builtin`.
    Builtin !Info !BuiltinOp
  | -- A data constructor (the function that creates the data).
    Constructor !Info !Tag
  | ConstValue !Info !Constant
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
    -- branches default`. `Case` is lazy: only the selected branch is evaluated.
    Case !Info !Node ![CaseBranch] !(Maybe Node)
  | -- Lazy `if` on booleans. It is reasonable to separate booleans from general
    -- datatypes for the purposes of evaluation and code generation.
    If !Info !Node !Node !Node
  | -- Evaluation only: evaluated data constructor (the actual data). Arguments
    -- order: right to left.
    Data !Info !Tag ![Node]
  | -- Evaluation only: `LambdaClosure env body`
    LambdaClosure !Info !Env !Node
  | -- Evaluation only: a suspended term value which cannot be evaluated
    -- further, e.g., a hole applied to some arguments. The suspended term must
    -- be closed.
    Suspended !Info !Node

-- Other things we might need in the future:
-- - laziness annotations (converting these to closure/thunk creation should be
--   done further down the pipeline)
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

-- `CaseBranch tag argsNum branch`
-- - `argsNum` is the number of arguments of the constructor tagged with `tag`,
--   equal to the number of implicit binders above `branch`
data CaseBranch = CaseBranch !Tag !Int !Node

-- all nodes in an environment must be closed (no free variables, i.e., no de
-- Bruijn indices pointing outside the term)
type Env = [Node]

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

unfoldLambdas :: Node -> ([Info], Node)
unfoldLambdas = go []
  where
    go :: [Info] -> Node -> ([Info], Node)
    go acc n = case n of
      Lambda i b -> go (i : acc) b
      _ -> (acc, n)

-- `NodeInfo` is a convenience datatype which provides the most commonly needed
-- information about a node in a generic fashion.
data NodeInfo = NodeInfo
  { -- `nodeInfo` is the info associated with the node,
    _nodeInfo :: Info,
    -- `nodeChildren` are the children, in a fixed order, i.e., the immediate
    -- recursive occurrences of Node
    _nodeChildren :: [Node],
    -- `nodeChildBindersNum` is the number of binders introduced for each child
    -- in the parent node. Same length and order as in `nodeChildren`.
    _nodeChildBindersNum :: [Int],
    -- `nodeChildBindersInfo` is information about binders for each child, if
    -- present. Same length and order as in `nodeChildren`.
    _nodeChildBindersInfo :: [Maybe [BinderInfo]],
    -- `nodeReassemble` reassembles the node from the info and the children
    -- (which should be in the same fixed order as in the `nodeChildren`
    -- component).
    _nodeReassemble :: Info -> [Node] -> Node
  }

makeLenses ''NodeInfo

-- destruct a node into NodeInfo
destruct :: Node -> NodeInfo
destruct = \case
  Var i idx -> NodeInfo i [] [] [] (\i' _ -> Var i' idx)
  Ident i sym -> NodeInfo i [] [] [] (\i' _ -> Ident i' sym)
  Builtin i op -> NodeInfo i [] [] [] (\i' _ -> Builtin i' op)
  Constructor i tag -> NodeInfo i [] [] [] (\i' _ -> Constructor i' tag)
  ConstValue i c -> NodeInfo i [] [] [] (\i' _ -> ConstValue i' c)
  Hole i -> NodeInfo i [] [] [] (\i' _ -> Hole i')
  Axiom i -> NodeInfo i [] [] [] (\i' _ -> Axiom i')
  App i l r -> NodeInfo i [l, r] [0, 0] [Nothing, Nothing] (\i' args' -> App i' (hd args') (args' !! 1))
  Lambda i b -> NodeInfo i [b] [1] [fetchBinderInfo i] (\i' args' -> Lambda i' (hd args'))
  LetIn i v b -> NodeInfo i [v, b] [0, 1] [Nothing, fetchBinderInfo i] (\i' args' -> LetIn i' (hd args') (args' !! 1))
  Case i v bs Nothing ->
    NodeInfo
      i
      (v : map (\(CaseBranch _ _ br) -> br) bs)
      (0 : map (\(CaseBranch _ k _) -> k) bs)
      (Nothing : fetchCaseBinderInfo i (replicate (length bs) Nothing))
      ( \i' args' ->
          Case
            i'
            (hd args')
            ( zipWithExact
                (\(CaseBranch tag k _) br' -> CaseBranch tag k br')
                bs
                (tl args')
            )
            Nothing
      )
  Case i v bs (Just def) ->
    NodeInfo
      i
      (v : def : map (\(CaseBranch _ _ br) -> br) bs)
      (0 : 0 : map (\(CaseBranch _ k _) -> k) bs)
      (Nothing : Nothing : fetchCaseBinderInfo i (replicate (length bs) Nothing))
      ( \i' args' ->
          Case
            i'
            (hd args')
            ( zipWithExact
                (\(CaseBranch tag k _) br' -> CaseBranch tag k br')
                bs
                (tl (tl args'))
            )
            (Just (hd (tl args')))
      )
  If i v b1 b2 ->
    NodeInfo
      i
      [v, b1, b2]
      [0, 0, 0]
      [Nothing, Nothing, Nothing]
      (\i' args' -> If i' (hd args') (args' !! 1) (args' !! 2))
  Data i tag args ->
    NodeInfo i args (map (const 0) args) (map (const Nothing) args) (`Data` tag)
  LambdaClosure i env b ->
    NodeInfo
      i
      (b : env)
      (1 : map (const 0) env)
      (fetchBinderInfo i : map (const Nothing) env)
      (\i' args' -> LambdaClosure i' (tl args') (hd args'))
  Suspended i t ->
    NodeInfo i [t] [0] [Nothing] (\i' args' -> Suspended i' (hd args'))
  where
    fetchBinderInfo :: Info -> Maybe [BinderInfo]
    fetchBinderInfo i = case Info.lookup kBinderInfo i of
      Just bi -> Just [bi]
      Nothing -> Nothing

    fetchCaseBinderInfo :: Info -> [Maybe [BinderInfo]] -> [Maybe [BinderInfo]]
    fetchCaseBinderInfo i d = case Info.lookup kCaseBinderInfo i of
      Just cbi -> map Just (cbi ^. infoBranchBinders)
      Nothing -> d

children :: Node -> [Node]
children = (^. nodeChildren) . destruct

-- children together with the number of binders
bchildren :: Node -> [(Int, Node)]
bchildren n =
  let ni = destruct n
   in zipExact (ni ^. nodeChildBindersNum) (ni ^. nodeChildren)

-- shallow children: not under binders
schildren :: Node -> [Node]
schildren = map snd . filter (\p -> fst p == 0) . bchildren

getInfo :: Node -> Info
getInfo = (^. nodeInfo) . destruct

modifyInfoM :: Applicative m => (Info -> m Info) -> Node -> m Node
modifyInfoM f n =
  let ni = destruct n
   in do
        i' <- f (ni ^. nodeInfo)
        return ((ni ^. nodeReassemble) i' (ni ^. nodeChildren))

modifyInfo :: (Info -> Info) -> Node -> Node
modifyInfo f n = runIdentity $ modifyInfoM (pure . f) n

{---------------------------------------------------------------------------------}
{- General recursors on Node  -}

-- Note: In the (distant) future, with dependent types, the type information
-- will contain Nodes. Then mapping/folding needs to be performed also on the
-- Nodes stored as type information.

-- a collector collects information top-down on a single path in the program
-- tree
data Collector a c = Collector
  { _cEmpty :: c,
    _cCollect :: a -> c -> c
  }

makeLenses ''Collector

unitCollector :: Collector a ()
unitCollector = Collector () (\_ _ -> ())

binderInfoCollector :: Collector (Int, Maybe [BinderInfo]) [Maybe BinderInfo]
binderInfoCollector =
  Collector
    []
    (\(k, bi) c -> if k == 0 then c else map Just (fromJust bi) ++ c)

binderNumCollector :: Collector (Int, Maybe [BinderInfo]) Index
binderNumCollector = Collector 0 (\(k, _) c -> c + k)

-- `umapG` maps the nodes bottom-up, i.e., when invoking the mapper function the
-- recursive subnodes have already been mapped
umapG ::
  forall c m.
  Monad m =>
  Collector (Int, Maybe [BinderInfo]) c ->
  (c -> Node -> m Node) ->
  Node ->
  m Node
umapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m Node
    go c n =
      let ni = destruct n
       in do
            ns <-
              sequence $
                zipWith3Exact
                  (\n' k bis -> go ((coll ^. cCollect) (k, bis) c) n')
                  (ni ^. nodeChildren)
                  (ni ^. nodeChildBindersNum)
                  (ni ^. nodeChildBindersInfo)
            f c ((ni ^. nodeReassemble) (ni ^. nodeInfo) ns)

umapM :: Monad m => (Node -> m Node) -> Node -> m Node
umapM f = umapG unitCollector (const f)

umapMB :: Monad m => ([Maybe BinderInfo] -> Node -> m Node) -> Node -> m Node
umapMB f = umapG binderInfoCollector f

umapMN :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
umapMN f = umapG binderNumCollector f

umap :: (Node -> Node) -> Node -> Node
umap f n = runIdentity $ umapM (return . f) n

umapB :: ([Maybe BinderInfo] -> Node -> Node) -> Node -> Node
umapB f n = runIdentity $ umapMB (\is -> return . f is) n

umapN :: (Index -> Node -> Node) -> Node -> Node
umapN f n = runIdentity $ umapMN (\idx -> return . f idx) n

-- `dmapG` maps the nodes top-down
dmapG ::
  forall c m.
  Monad m =>
  Collector (Int, Maybe [BinderInfo]) c ->
  (c -> Node -> m Node) ->
  Node ->
  m Node
dmapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m Node
    go c n = do
      n' <- f c n
      let ni = destruct n'
      ns <-
        sequence $
          zipWith3Exact
            (\n'' k bis -> go ((coll ^. cCollect) (k, bis) c) n'')
            (ni ^. nodeChildren)
            (ni ^. nodeChildBindersNum)
            (ni ^. nodeChildBindersInfo)
      return ((ni ^. nodeReassemble) (ni ^. nodeInfo) ns)

dmapM :: Monad m => (Node -> m Node) -> Node -> m Node
dmapM f = dmapG unitCollector (const f)

dmapMB :: Monad m => ([Maybe BinderInfo] -> Node -> m Node) -> Node -> m Node
dmapMB f = dmapG binderInfoCollector f

dmapMN :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
dmapMN f = dmapG binderNumCollector f

dmap :: (Node -> Node) -> Node -> Node
dmap f n = runIdentity $ dmapM (return . f) n

dmapB :: ([Maybe BinderInfo] -> Node -> Node) -> Node -> Node
dmapB f n = runIdentity $ dmapMB (\is -> return . f is) n

dmapN :: (Index -> Node -> Node) -> Node -> Node
dmapN f n = runIdentity $ dmapMN (\idx -> return . f idx) n

-- `ufoldG` folds the tree bottom-up. The `uplus` argument combines the values -
-- it should be commutative and associative.
ufoldG ::
  forall c a m.
  Monad m =>
  Collector (Int, Maybe [BinderInfo]) c ->
  (a -> a -> a) ->
  (c -> Node -> m a) ->
  Node ->
  m a
ufoldG coll uplus f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m a
    go c n = foldr (liftM2 uplus) (f c n) mas
      where
        ni :: NodeInfo
        ni = destruct n
        mas :: [m a]
        mas =
          zipWith3Exact
            (\n' k bis -> go ((coll ^. cCollect) (k, bis) c) n')
            (ni ^. nodeChildren)
            (ni ^. nodeChildBindersNum)
            (ni ^. nodeChildBindersInfo)

ufoldM :: Monad m => (a -> a -> a) -> (Node -> m a) -> Node -> m a
ufoldM uplus f = ufoldG unitCollector uplus (const f)

ufoldMB :: Monad m => (a -> a -> a) -> ([Maybe BinderInfo] -> Node -> m a) -> Node -> m a
ufoldMB uplus f = ufoldG binderInfoCollector uplus f

ufoldMN :: Monad m => (a -> a -> a) -> (Index -> Node -> m a) -> Node -> m a
ufoldMN uplus f = ufoldG binderNumCollector uplus f

ufold :: (a -> a -> a) -> (Node -> a) -> Node -> a
ufold uplus f n = runIdentity $ ufoldM uplus (return . f) n

ufoldB :: (a -> a -> a) -> ([Maybe BinderInfo] -> Node -> a) -> Node -> a
ufoldB uplus f n = runIdentity $ ufoldMB uplus (\is -> return . f is) n

ufoldN :: (a -> a -> a) -> (Index -> Node -> a) -> Node -> a
ufoldN uplus f n = runIdentity $ ufoldMN uplus (\idx -> return . f idx) n

walk :: Monad m => (Node -> m ()) -> Node -> m ()
walk = ufoldM mappend

walkB :: Monad m => ([Maybe BinderInfo] -> Node -> m ()) -> Node -> m ()
walkB = ufoldMB mappend

walkN :: Monad m => (Index -> Node -> m ()) -> Node -> m ()
walkN = ufoldMN mappend

gather :: (a -> Node -> a) -> a -> Node -> a
gather f acc n = fst $ run $ runState acc (walk (\n' -> modify (`f` n')) n)

gatherB :: ([Maybe BinderInfo] -> a -> Node -> a) -> a -> Node -> a
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

convertClosures :: Node -> Node
convertClosures = umap go
  where
    go :: Node -> Node
    go n = case n of
      LambdaClosure i env b -> substEnv env (Lambda i b)
      _ -> n

convertData :: Node -> Node
convertData = umap go
  where
    go :: Node -> Node
    go n = case n of
      Data i tag args -> mkApp' (Constructor i tag) args
      _ -> n

convertSuspended :: Node -> Node
convertSuspended = umap go
  where
    go :: Node -> Node
    go n = case n of
      Suspended _ t -> t
      _ -> n

convertRuntimeNodes :: Node -> Node
convertRuntimeNodes = convertSuspended . convertData . convertClosures
