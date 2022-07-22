module Juvix.Core.GNode where

{-
  This file defines the graph representation of JuvixCore (GNode datatype) and
  general recursors on it.
-}

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
type Index = Word

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
  | App !i !(GNode i) ![GNode i]
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

-- i: info type
-- a: top-down accumulator type
-- b: result type (bottom-up accumulator)
data GNodeSig i a b = GNodeSig
  { _fVar :: a -> i -> Index -> b,
    _fIdent :: a -> i -> Symbol -> b,
    _fBuiltin :: a -> i -> Symbol -> b,
    _fConstInteger :: a -> i -> Integer -> b,
    _fConstBool :: a -> i -> Bool -> b,
    _fConstHole :: a -> i -> b,
    _fApp :: a -> i -> GNode i -> b -> [GNode i] -> [b] -> b,
    _fLambda :: a -> i -> GNode i -> b -> b,
    _fLetIn :: a -> i -> GNode i -> b -> GNode i -> b -> b,
    _fData :: a -> i -> Tag -> [GNode i] -> [b] -> b,
    _fCase :: a -> i -> GNode i -> b -> [CaseBranch i] -> [b] -> b,
    _fLambdaClosure :: a -> i -> [GNode i] -> [b] -> GNode i -> b -> b
  }

makeLenses ''GNodeSig

-- `recurse f sig acc` recurses through the graph, using `sig` to accumulate
-- results bottom-up, `f` to accumulate values top-down on the current path with
-- `a` the initial top-down accumulator value
recurse :: GNodeSig i a b -> (a -> GNode i -> a) -> a -> GNode i -> b
recurse sig f a n = case n of
  Var i idx -> (sig ^. fVar) a i idx
  Ident i sym -> (sig ^. fIdent) a i sym
  Builtin i sym -> (sig ^. fBuiltin) a i sym
  Literal i (ConstInteger int) -> (sig ^. fConstInteger) a i int
  Literal i (ConstBool b) -> (sig ^. fConstBool) a i b
  Literal i ConstHole -> (sig ^. fConstHole) a i
  App i l args -> (sig ^. fApp) a i l (goRec l) args (map goRec args)
  Lambda i body -> (sig ^. fLambda) a i body (goRec body)
  LetIn i value body -> (sig ^. fLetIn) a i value (goRec value) body (goRec body)
  Data i tag args -> (sig ^. fData) a i tag args (map goRec args)
  Case i value branches -> (sig ^. fCase) a i value (goRec value) branches (map (\(CaseBranch _ br) -> goRec br) branches)
  LambdaClosure i env body -> (sig ^. fLambdaClosure) a i env (map goRec env) body (goRec body)
  where
    goRec = recurse sig f (f a n)

-- recurse with binding info
recurseWithBindingInfo :: i' -> (i -> i' -> i') -> GNodeSig i (i', a) b -> (i' -> a -> GNode i -> a) -> a -> GNode i -> b
recurseWithBindingInfo nil cs sig f acc = recurse sig f' (nil, acc)
  where
    f' (is, a) n = case n of
      Lambda i _ -> (cs i is, f is a n)
      LetIn i _ _ -> (cs i is, f is a n)
      LambdaClosure i _ _ -> (cs i is, f is a n)
      _ -> (is, f is a n)

recurseB :: GNodeSig i ([i], a) b -> ([i] -> a -> GNode i -> a) -> a -> GNode i -> b
recurseB = recurseWithBindingInfo [] (:)

recurseN :: GNodeSig i (Int, a) b -> (Int -> a -> GNode i -> a) -> a -> GNode i -> b
recurseN = recurseWithBindingInfo 0 (const (+ 1))

nmapSig :: (a -> GNode i -> GNode i) -> GNodeSig i a (GNode i)
nmapSig f =
  GNodeSig
    {
      _fVar = \a i idx -> f a (Var i idx),
      _fIdent = \a i sym -> f a (Ident i sym),
      _fBuiltin = \a i sym -> f a (Ident i sym),
      _fConstInteger = \a i int -> f a (Literal i (ConstInteger int)),
      _fConstBool = \a i b -> f a (Literal i (ConstBool b)),
      _fConstHole = \a i -> f a (Literal i ConstHole),
      _fApp = \a i _ l' _ args' -> f a (App i l' args'),
      _fLambda = \a i _ body' -> f a (Lambda i body'),
      _fLetIn = \a i _ value' _ body' -> f a (LetIn i value' body'),
      _fData = \a i tag _ args' -> f a (Data i tag args'),
      _fCase = \a i _ value' bs bs' -> f a (Case i value' (zipWithExact (\(CaseBranch tag _) br' -> CaseBranch tag br') bs bs')),
      _fLambdaClosure = \a i _ env' _ body' -> f a (LambdaClosure i env' body')
    }

nmap :: (GNode i -> GNode i) -> GNode i -> GNode i
nmap f = recurse (nmapSig (const f)) (\_ _ -> ()) ()

nmapB :: ([i] -> GNode i -> GNode i) -> GNode i -> GNode i
nmapB f = recurseB (nmapSig (f . fst)) (\_ _ _ -> ()) ()

nmapN :: (Int -> GNode i -> GNode i) -> GNode i -> GNode i
nmapN f = recurseN (nmapSig (f . fst)) (\_ _ _ -> ()) ()
