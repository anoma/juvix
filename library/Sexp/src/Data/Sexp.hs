-- | This module serves as the main sexpression import it contains the
-- sexp type and all the various helper functionality one can need.
-- The star affix stands for a play on a theme. In this case the theme
-- is that they recurse on the structure itself calling itself on very
-- nested structures.
module Data.Sexp
  ( module Data.Sexp.Types,
    module Data.Sexp.Parser,
    module Data.Sexp.Serialize,
    B,
    Opt (..),

    -- * Traversal Functionality
    mapPredStar,
    traversePredStar,
    traversePredOptStar,
    autoRecurse,
    map,
    traverse,
    foldM,
    foldr,
    foldr1,

    -- * Partial Serialization Functionality
    Serialize (..),
    partiallyDeserialize,
    fullySerialize,
    withSerialization,
    mapSerialize,
    traverseSerialize,
    transformSerialize,
    onSerialize,
    traverseOnAtoms,
    mapOnAtoms,

    -- * General Functionality
    butLast,
    last,
    init,
    list,
    listStar,
    addMetaToCar,
    car,
    cdr,
    primOp,
    atom,
    actualAtom,
    suffixAtom,
    number,
    double,
    string,
    isAtomNamed,
    nameFromT,
    atomFromT,
    atomErr,
    doubleFromT,
    stringFromT,
    groupBy2,
    assoc,
    cadr,
    unGroupBy2,
    snoc,
    findKey,
    flatten,
  )
where

import Mari.Library hiding (foldM, foldr, init, list, map, reverse, show, toList, traverse)
import qualified Mari.Library as Std
import qualified Mari.Library.NameSymbol as NameSymbol
import Data.Sexp.Parser
import Data.Sexp.Serialize
import Data.Sexp.Types hiding (double)
import Prelude (error)

-- Just for Ease of writing
type B a = Base a

-- | @Opt@ is a record controlling some extra features for some choice
-- functions
data Opt a b = Op
  { atomF :: Maybe (Atom a -> Atom b),
    onAtom :: Bool
  }
  deriving (Show)

instance Semigroup (Opt a b) where
  Op f onF <> Op g onB = Op (f <|> g) (onF || onB)

instance Monoid (Opt a b) where
  mempty = Op Nothing False

--------------------------------------------------------------------------------
-- Manual Serializing instances
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Folding Capabilities
--------------------------------------------------------------------------------

-- | @traversePredStar@ is like @mapPredStar@ with some notable exceptions.
-- 1. The @f@ function now takes an extra argument, recurse, which will
--    recurse the given form with @traversePredOptStar@
-- 2. Instead of recusing on the changed match expression automatically,
--    it must be done through @autoRecurse@ or calling the extra recursion
--    function automatically
-- 3. If the @Opt@ function has the @onAtom@ field being true
--    then the function will also be ran on the atom
traversePredOptStar ::
  Monad f =>
  B a ->
  (NameSymbol.T -> Bool) ->
  (B a -> (B a -> f (B a)) -> f (B a)) ->
  Opt a a ->
  f (B a)
traversePredOptStar t predChange f opt =
  case t of
    Cons (Atom (A name _)) _
      | predChange name ->
        f t recurseDown
    Cons {} -> traverse recurseDown t
    Nil {} -> pure Nil
    Atom a
      | onAtom opt -> f t recurseDown
      | otherwise -> pure (Atom a)
  where
    recurseDown sexp = traversePredOptStar sexp predChange f opt

-- | @traversePredStar@ is just like @traversePredOptStar@ except the
-- empty option set is given, meaning that the function will not match
-- on atoms.
traversePredStar ::
  Monad f => B a -> (NameSymbol.T -> Bool) -> (B a -> (B a -> f (B a)) -> f (B a)) -> f (B a)
traversePredStar t predChange automaticF =
  traversePredOptStar t predChange automaticF mempty

autoRecurse ::
  Monad m => (B a -> m (B a)) -> B a -> (B a -> m (B a)) -> m (B a)
autoRecurse automaticF xs recurse = do
  newCons <- automaticF xs
  case newCons of
    Cons {} -> traverse recurse newCons
    _______ -> pure newCons

-- | @mapPredStar@ searches the sexp structure given to it with a
-- given predicate. This predicate is ran on the @car@ of every list
-- structure. This simulates searches for the head of a function
-- call. When the predicate returns true, the function passed to
-- @mapPredStar@ (which we shall refer to as f), is then called on the
-- @car@, and the @cdr@ of the list, giving back a new sexp
-- structure. This new sexp structure is then recursed upon by
-- @mapPredStar@. NOTE that this does mean the structure handed back
-- by f.
mapPredStar :: B a -> (NameSymbol.T -> Bool) -> (B a -> B a) -> B a
mapPredStar t pred f =
  case t of
    Cons (Atom (A name _)) _
      | pred name -> mapPredStar (f t) pred f
    Cons _ _ -> map (\t -> mapPredStar t pred f) t
    Atom ato -> Atom ato
    Nil -> Nil

traverse :: Monad m => (B a -> m (B b)) -> B a -> m (B b)
traverse f xs =
  foldM (\acc x -> Cons <$> f x <*> pure acc) Nil xs
    >>| reverse

foldM :: Monad m => (p -> B a -> m p) -> p -> B a -> m p
foldM f acc ts =
  case ts of
    Cons a as -> f acc a >>= (\accum -> foldM f accum as)
    Atom ____ -> f acc ts
    Nil -> pure acc

-- | @map@ works the same as it does in Haskell. However instead of
-- running it on the generic of the Sexp, it instead runs it on the
-- elements on the Sexp structure itself. Thus the @a -> b@ signature
-- can only occur if the function recuses to change the sexp
-- recursively
map :: (B a -> B b) -> B a -> B b
map f = foldr (Cons . f) Nil

-- | @foldr@ works the same way as it does in Haskell. If the list is
-- terminated improperly with an atom (called a dotted list), that is
-- considered to be the last element of the list, as if it weren't a
-- dotted list.
foldr :: (B a -> p -> p) -> p -> B a -> p
foldr f acc ts =
  case ts of
    Cons a as -> f a (foldr f acc as)
    Atom ____ -> f ts acc
    Nil -> acc

-- | @foldr1@ works the same as it does in Haskell. Upon a dotted list,
-- it behaves like foldr. If the list is empty or nil, we return nothing
foldr1 :: (B a -> B a -> B a) -> B a -> Maybe (B a)
foldr1 f (Cons x xs) = Just $ unsafe (Cons x xs)
  where
    unsafe ts =
      case ts of
        Cons a Nil -> a
        Cons a cds -> f a (unsafe cds)
        Atom a -> (Atom a)
        Nil -> error "doesn't happen"
foldr1 _ _empty = Nothing

--------------------------------------------------------------------------------
-- Partial Serialization Functions
--------------------------------------------------------------------------------

-- | @partiallyDeserialize@ serializes a S-expression term with no
-- serialization, into a de-serialization over a generic a. It is the
-- responsibility of the partial deserializer itself to deal with the
-- arguments.
partiallyDeserialize :: forall a. Serialize a => T -> B a
partiallyDeserialize Nil = Nil
partiallyDeserialize sexp@Cons {} =
  case deserialize sexp of
    Just partial ->
      Atom (P partial Nothing)
    Nothing -> map partiallyDeserialize sexp
partiallyDeserialize atom@(Atom p) =
  case deserialize atom of
    Just serialize ->
      Atom (P serialize Nothing)
    Nothing ->
      case p of
        -- this should never happen, as all P's have no meaning with T
        P _ _ -> Nil
        D n l -> Atom (D n l)
        S n l -> Atom (S n l)
        N n l -> Atom (N n l)
        A n l -> Atom (A n l)

-- | @fullySerialize@ removes the partial deserialization of a
-- structure, into the fully serialized form.
fullySerialize :: forall a. Serialize a => B a -> T
fullySerialize Nil = Nil
fullySerialize sexp@Cons {} =
  map fullySerialize sexp
fullySerialize (Atom p) =
  case p of
    P t _ -> serialize t
    D n l -> Atom (D n l)
    S n l -> Atom (S n l)
    N n l -> Atom (N n l)
    A n l -> Atom (A n l)

-- | @withSerialization@ Temporarily de-serializes the given
-- s-expression
withSerialization ::
  forall a m. (Monad m, Serialize a) => T -> (B a -> m (B a)) -> m T
withSerialization sexp func =
  partiallyDeserialize sexp
    |> func
    >>| fullySerialize

---------------------------------------------
-- Partial Serialization Mapping Functions
---------------------------------------------

-- | @mapSerialize@ traverses the list like a star function, searching
-- for any constructor of the partial deserialization to apply the
-- given function to. The function takes two arguments. the
-- deserialized structure itself, and a recursive function to call for
-- any nested Sexp structures that may exist in the partial deserialization
mapSerialize :: B a -> (a -> (B a -> B b) -> b) -> B b
mapSerialize xs f = transformSerialize xs newF
  where
    newF sexp rec' = Atom (P (f sexp rec') Nothing)

-- | @traverseSerialize@ works just like @mapSerialize@ except the
-- function is effectful
traverseSerialize :: Monad m => B a -> (a -> (B a -> m (B b)) -> m b) -> m (B b)
traverseSerialize xs f = onSerialize xs newF
  where
    newF x rec' = Atom . (`P` Nothing) <$> f x rec'

-- | @onSerialize@ acts like @traverseSerialize@ however, instead of
-- returning the deserialized it returns an arbitrary S-expression
-- that contains the deserialization. Overall this function is more
-- general than @traverseSerialize@ and should be used when one wants
-- to potentially return an s-expression and not a deserialized form
onSerialize :: Monad m => B a -> (a -> (B a -> m (B b)) -> m (B b)) -> m (B b)
onSerialize xs f = traverseOnAtoms xs newF
  where
    newF a rec' =
      case a of
        P t _ -> f t rec'
        D n l -> pure $ Atom (D n l)
        S n l -> pure $ Atom (S n l)
        N n l -> pure $ Atom (N n l)
        A n l -> pure $ Atom (A n l)

-- | @transformSerialize@ acts like @mapSerialize@ however, instead of
-- returning the deserialized it returns an arbitrary S-expression
-- that contains the deserialization. Overall this function is more
-- general than @mapSerialize@ and should be used when one wants to
-- potentially return an s-expression and not a deserialized form
transformSerialize :: B a -> (a -> (B a -> B b) -> B b) -> B b
transformSerialize xs f = runIdentity (onSerialize xs newF)
  where
    newF sexp rec' = pure $ f sexp (runIdentity . rec')

-- | @traverseOnAtoms@ is like @onSerialize@, however it executes on
-- all atoms instead of just the serialized data. This is useful for
-- operating on Symbols, and partial de-serializations at the same
-- time.
traverseOnAtoms ::
  Monad f => B a -> (Atom a -> (B a -> f (B b)) -> f (B b)) -> f (B b)
traverseOnAtoms xs@Cons {} f = traverse (`traverseOnAtoms` f) xs
traverseOnAtoms Nil _______f = pure Nil
traverseOnAtoms (Atom a) f = f a (`traverseOnAtoms` f)

-- | @mapOnAtoms@ is a pure vesrion of @traverseOnAtoms@
mapOnAtoms :: B a -> (Atom a -> (B a -> B b) -> (B b)) -> B b
mapOnAtoms t f =
  runIdentity (traverseOnAtoms t newF)
  where
    newF xs rec' = pure (f xs (runIdentity . rec'))

--------------------------------------------------------------------------------
-- General Functionality
--------------------------------------------------------------------------------

reverse :: B a -> B a
reverse xs = go xs Nil
  where
    go (Cons a as) acc = go as (Cons a acc)
    go Nil acc = acc
    go (Atom a) acc = Cons (Atom a) acc

-- | @butLast@ takes a list and removes the last element of the list,
-- if handed an atom, it will return the atom
butLast :: B a -> B a
butLast (Cons _ Nil) = Nil
butLast (Cons x xs) = Cons x (butLast xs)
butLast (Atom a) = Atom a
butLast Nil = Nil

-- | @last@ gives back the last element of the list, for an atom or nil
-- it will be the identity
last :: B a -> B a
last (Cons x Nil) = x
last (Cons _ xs) = last xs
last (Atom a) = Atom a
last Nil = Nil

-- | @init@ gives back the list back minus the last element, for an atom or nil
-- it will be the identity
init :: B a -> B a
init (Cons _ Nil) = Nil
init (Cons x xs) = Cons x (init xs)
init (Atom a) = Atom a
init Nil = Nil

-- | @list@ takes a foldable structure of Sexps and gives back a list of
-- those structures
list :: Foldable t => t (B a) -> B a
list = Std.foldr Cons Nil

-- | @listStar@ is a lot like @list@, but, it will automatically @Cons@
-- into the last structure
-- Example:
-- >>> listStar [atom "list", atom "a", atom "b", list [atom "c", atom "d"]]
-- ("list" "a" "b" "c" "d")
listStar :: [B a] -> B a
listStar = fromMaybe Nil . foldr1May Cons

-- | @addMetaToCar@ moves the meta information from a given atom into
-- the car of the given sexpression. If it's not a @Cons@ then it is
-- ignored.
addMetaToCar :: Atom a -> B a -> B a
addMetaToCar (A _ lineInfo) (Cons (Atom (A term _)) xs) =
  Cons (Atom (A term lineInfo)) xs
addMetaToCar _ xs = xs

-- | @car@ grabs the head of the list
car :: B a -> B a
car (Cons x _) = x
car Nil = Nil
car (Atom a) = Atom a

-- | @cdr@ grabs the tail of the list
cdr :: B a -> B a
cdr (Cons _ xs) = xs
cdr Nil = Nil
cdr (Atom a) = Atom a

-- | @cadr@ grabs the second element of the list
cadr :: B a -> B a
cadr = car . cdr

-- | @primOp@ creates a deserialized value in a @Sexp@
primOp :: a -> B a
primOp x = Atom $ P x Nothing

-- | @atom@ creates a @Sexp@ @Atom@ from a @NameSymbol.T@
atom :: NameSymbol.T -> B a
atom x = Atom $ A x Nothing

-- | @actualAtom@ creates an @Atom@ from a @NameSymbol.T@
actualAtom :: NameSymbol.T -> Atom a
actualAtom x = A x Nothing

suffixAtom :: NameSymbol.T -> B a -> B a
suffixAtom name (Atom (A name' cdr)) = (Atom (A (NameSymbol.append name' name) cdr))
suffixAtom _ sexp = sexp

-- | @number@ creates a @Sexp@ @Number@ from an @Integer@
number :: Integer -> B a
number n = Atom $ N n Nothing

-- | @double@ creates a @Sexp@ @Double@ from a @Double@
double :: Double -> B a
double d = Atom $ D d Nothing

-- | @string@ creates a @Sexp@ @String@ from a @Text@
string :: Text -> B a
string t = Atom $ S t Nothing

-- | @isAtomNamed@ asks if an atom is named a particular name. Cons and
-- Nil both return False
isAtomNamed :: B a -> NameSymbol.T -> Bool
isAtomNamed (Atom (A name _)) name2 = name == name2
isAtomNamed _ _ = False

atomErr :: B a -> Atom a
atomErr (Atom a) = a
atomErr _ = error "the value is not an atom"

-- | @atomFromT@ returns the Atom from the list, will returning
-- @Nothing@ if it is not an @Atom@
atomFromT :: B a -> Maybe (Atom a)
atomFromT (Atom a) = Just a
atomFromT _ = Nothing

-- | @nameFromT@ is similar to @atomFromT@ but it grabs the
-- @NameSymbol.T@ out of the @Atom@
nameFromT :: B a -> Maybe NameSymbol.T
nameFromT (Atom (A name _)) = Just name
nameFromT _ = Nothing

-- | @doubleFromT@ is similar to @atomFromT@ but it grabs the
-- @dobule@ out of the @Atom@
doubleFromT :: B a -> Maybe Double
doubleFromT (Atom (D d _)) = Just d
doubleFromT _ = Nothing

-- | @stringFromT@ is similar to @atomFromT@ but it grabs the
-- @string@ out of the @Atom@
stringFromT :: B a -> Maybe Text
stringFromT (Atom (S s _)) = Just s
stringFromT _ = Nothing

-- | @assoc@ takes two sexps. First being a sexp that is to be found in
-- the second argument. The other is a sexp list that is grouped into
-- twos. If the search for element is found in the grouped list, then
-- the second element of that sexp is returned. Otherwise @Nothing@ is
-- returned.
-- Example:
-- >>> fmap (assoc (number 2)) (parse "((1 a) (2 b) (3 c))")
-- Right (Just "b")
assoc :: Eq a => B a -> B a -> Maybe (B a)
assoc t xs = cadr <$> findKey car t xs

-- | @groupBy2@ groups the given sexp structure into pairs. If the list
-- is not even, then the last element is dropped.
-- Example
-- >>> fmap groupBy2 (parse "(1 a 2 b 3 c)")
-- Right ((1 "a") (2 "b") (3 "c"))
groupBy2 :: B a -> B a
groupBy2 (a1 :> a2 :> rest) =
  list [a1, a2] :> groupBy2 rest
groupBy2 _ = Nil

-- | @unGroupBy2@ does the opposite of @groupBy2@. If the given list is
-- even then
-- @unGroupBy2 . groupBy2 = idl@ ∧ @groupBy2 .  unGroupBy2 = idr@
unGroupBy2 :: B a -> B a
unGroupBy2 (List [a1, a2] :> rest) =
  a1 :> a2 :> unGroupBy2 rest
unGroupBy2 (a :> rest) =
  a :> unGroupBy2 rest
unGroupBy2 a = a

-- | @snoc@ is cons but backwards. Thus it conses on the end of a given list
snoc :: B a -> B a -> B a
snoc e (Cons x y) = Cons x (snoc e y)
snoc e a@(Atom _) = Cons a e
snoc e Nil = e

-- | @findKey searches a Sexpression structure for a particular
-- structure. the given key allows us to zoom in on a particular subset
-- of that structure.
-- >>> fmap (findKey car (number 2)) (parse "((1 a) (2 b) (3 c))")
-- Right (Just (2 "b"))
findKey :: Eq a => (B a -> B a) -> B a -> B a -> Maybe (B a)
findKey f k (x :> xs)
  | f x == k = Just x
  | otherwise = findKey f k xs
findKey _ _ _ = Nothing

-- | @flatten@ totally flattens a list, removing any extra Nils as well
-- >>> fmap flatten (parse "((1) (2 3 4) (1 2) () (1 2 (3 ())))")
-- Right (1 2 3 4 1 2 1 2 3)
flatten :: B a -> B a
flatten xs = rec xs Nil
  where
    rec Nil acc = acc
    rec (Atom a) acc = Cons (Atom a) acc
    rec (Cons x xs) acc = rec x (rec xs acc)
