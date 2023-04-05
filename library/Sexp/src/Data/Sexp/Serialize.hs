{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | @Data.Sexp.Serialize@ serves as the automatic serialization of
-- Haskell Data-structures. One can derive a default instance by
-- simply having
--
-- @
-- Data Foo = Foo deriving(Generic)
-- @
--
-- then writing
--
-- @
-- instance DefaultOptions Foo
-- instance Serialize Foo
-- @
--
-- There are two parts to this derivation. The first is the
-- @Serialize@ derivation, which gives on the ability to write
-- @serialize@ and @deserialize@.
--
-- However one needs to also derive @DefaultOptions@ as, in order to
-- get an efficient serialization, default options are needed to
-- inform the system how it ought to serialize the structre. An
-- example of what the constructor renaming looks like as follows
-- @
-- data Foo = Foo Int | FooBar Int | Bar
--
-- -- Gets turned into
-- -- Foo 3
-- (:foo 3)
--
-- -- FooBar 5
-- (:foo-bar 5)
--
-- -- Bar
-- :bar
-- @
--
-- An example of giving a renaming is as follows
--
-- @
-- data Infix
--   = Infix
--       { infixOp :: NameSymbol.T,
--         infixLt :: (Sexp.B (Bind.BinderPlus Infix)),
--         infixInf :: Infix
--       }
--   | InfixNoMore
--       { infixOp :: NameSymbol.T,
--         infixLt :: (Sexp.B (Bind.BinderPlus Infix)),
--         infixRt :: (Sexp.B (Bind.BinderPlus Infix))
--       }
--   deriving (Show, Generic, Eq)
--
-- infixRename :: Sexp.Options
-- infixRename =
--   Sexp.changeName
--     (Sexp.defaultOptions @Infix)
--     (Map.fromList [("InfixNoMore", ":infix")])
--
-- instance Sexp.DefaultOptions Infix
--
-- instance Sexp.Serialize Infix where
--   serialize = Sexp.serializeOpt infixRename
--   deserialize = Sexp.deserializeOpt infixRename
-- @
--
-- In this example, we make an @Infix@ data structure with two almost
-- identical looking constructor and arguments. Indeed we state they
-- stand for the same s-expression, with one just having a chain of
-- infixs, we can see this by @infixRename@. Finally we derive the
-- serialization based on these consturctors. And thus in practice the
-- serializer will automatically give us a chain of @Infix@ before
-- ending in an @InfixNoMore@
module Data.Sexp.Serialize
  ( Serialize (..),
    DefaultOptions (..),
    Options (..),
    changeName,
    serializeOpt,
    deserializeOpt,
  )
where

import qualified Data.Char8 as Char8
import qualified Data.HashSet as Set
import GHC.Generics as Generics
import qualified Generics.Deriving.ConNames as ConNames
import Mari.Library hiding (foldr)
import qualified Mari.Library.HashMap as Map
import qualified Mari.Library.NameSymbol as NameSymbol
import Data.Sexp.Types as Sexp hiding (double)

class Serialize a where
  serialize :: a -> T
  default serialize :: (Generic a, GSerializeOptions (Rep a), DefaultOptions a) => a -> T
  serialize = serializeOpt (defaultOptions @a)

  deserialize :: T -> Maybe a
  default deserialize :: (Generic a, GSerializeOptions (Rep a), DefaultOptions a) => T -> Maybe a
  deserialize = deserializeOpt (defaultOptions @a)

data Options = Options
  { -- this decides if the constructors should be renamed, by default
    -- it maps them all to the lisp variants
    constructorMapping :: Map.T [Char] NameSymbol.T,
    consturctorNameSet :: Set.HashSet NameSymbol.T
  }
  deriving (Show)

class DefaultOptions a where
  defaultOptions :: Options
  default defaultOptions :: (Generic a, ConNames.ConNames (Rep a)) => Options
  defaultOptions = defaultOptions' @a

defaultOptions' :: forall a. (Generic a, ConNames.ConNames (Rep a)) => Options
defaultOptions' =
  Options map nameSet
  where
    nameMapping =
      ConNames.conNames @a undefined
    namesList =
      nameMapping
        |> fmap mlNameToReservedLispName
    nameSet =
      namesList
        |> Set.fromList
    map = Map.fromList (zip nameMapping namesList)

changeName :: Options -> Map.T [Char] NameSymbol.T -> Options
changeName (Options hashMapOpt _) hashmap =
  Options newMap (Map.elems newMap |> Set.fromList)
  where
    newMap = hashmap <> hashMapOpt

class GSerializeOptions f where
  gputOpt :: Options -> f a -> T
  ggetOpt :: Options -> T -> Maybe (T, (f a))

serializeOpt :: (GSerializeOptions (Rep a), Generic a) => Options -> a -> T
serializeOpt opt t =
  gputOpt opt (from t)

deserializeOpt :: (Generic b, GSerializeOptions (Rep b)) => Options -> T -> Maybe b
deserializeOpt opt t =
  to . snd <$> ggetOpt opt t

----------------------------------------
-- U1
----------------------------------------

instance GSerializeOptions U1 where
  gputOpt __ U1 = Nil
  ggetOpt _ xs = Just (xs, U1)

----------------------------------------
-- M1
----------------------------------------

instance (GSerializeOptions a) => GSerializeOptions (D1 i a) where
  gputOpt opt (M1 x) =
    gputOpt opt x
  ggetOpt opt xs =
    second M1 <$> ggetOpt opt xs

-- Make an alternative version that cares about the selector
-- constructor
instance (Selector i, GSerializeOptions a) => GSerializeOptions (S1 i a) where
  gputOpt opt (M1 x) =
    gputOpt opt x
  ggetOpt opt xs =
    -- see if car is correct
    second M1 <$> ggetOpt opt xs

-- can we make consturctors with no arguments not be a list!?  for
-- example if we have @| Test@ We want it not to be (:test), but :test
instance (Constructor i, GSerializeOptions a) => GSerializeOptions (C1 i a) where
  gputOpt opt y@(M1 x) =
    let name =
          case constructorMapping opt Map.!? (conName y) of
            Nothing ->
              -- Should never happen, as we bind all constructor names
              panic "we should never hit here"
            -- mlNameToReservedLispName (conName y)
            Just name ->
              name
     in case gputOpt opt x of
          Sexp.Nil -> Sexp.Atom (A name Nothing)
          otherwis -> Cons (Sexp.Atom (A name Nothing)) otherwis
  ggetOpt opt xs =
    case xs of
      Cons (Atom (A nameOf _)) _
        | check nameOf ->
          logic (ggetOpt opt (cdr xs)) nameOf
      Atom (A nameOfSingleCon _)
        | check nameOfSingleCon ->
          logic (ggetOpt opt Nil) nameOfSingleCon
      _ ->
        Nothing
    where
      -- still slow, we should check the M1 constructed agrees before
      -- even trying.
      check name =
        Set.member name (consturctorNameSet opt)
      logic caseOn name1 =
        case caseOn of
          Just (rest, t) ->
            let m1 = M1 t
                name =
                  case constructorMapping opt Map.!? (conName m1) of
                    Nothing ->
                      -- Should never happen, as we bind all constructor names
                      panic "we should never hit here"
                    -- mlNameToReservedLispName (conName m1)
                    Just name ->
                      name
             in if
                    | name1 == name -> Just (rest, m1)
                    | otherwise -> Nothing
          Nothing -> Nothing

----------------------------------------
-- Sum
----------------------------------------

instance (GSerializeOptions a, GSerializeOptions b) => GSerializeOptions (a :+: b) where
  gputOpt opt (L1 x) = gputOpt opt x
  gputOpt opt (R1 x) = gputOpt opt x

  -- is this correct?
  ggetOpt opt xs = (second L1 <$> ggetOpt opt xs) <|> (second R1 <$> ggetOpt opt xs)

instance (GSerializeOptions a, GSerializeOptions b) => GSerializeOptions (a :*: b) where
  gputOpt opt (a :*: b) = append (gputOpt opt a) (gputOpt opt b)

  -- is this correct also!?
  ggetOpt opt xs = do
    (ret, firstSetOfSlot) <- ggetOpt opt xs
    (rest, secondSetOfSlot) <- ggetOpt opt ret
    Just (rest, firstSetOfSlot :*: secondSetOfSlot)

----------------------------------------
-- K1
----------------------------------------

instance Serialize a => GSerializeOptions (K1 i a) where
  gputOpt _ (K1 x) = Sexp.Cons (serialize x) Nil
  ggetOpt _ xs =
    deserialize (car xs) >>| \des -> (cdr xs, K1 des)

mlNameToReservedLispName :: [Char] -> NameSymbol.T
mlNameToReservedLispName [] = NameSymbol.fromString ""
mlNameToReservedLispName (x : xs) =
  let upperToDash x
        | Char8.isUpper x = ['-', Char8.toLower x]
        | otherwise = [x]
      properName =
        xs >>= upperToDash
   in NameSymbol.fromString (':' : Char8.toLower x : properName)

----------------------------------------
-- Base Instances
----------------------------------------

instance Serialize a => Serialize (Base a) where
  serialize (Sexp.Cons x xs) = Sexp.Cons (serialize x) (serialize xs)
  serialize Sexp.Nil = Sexp.Nil
  serialize (Sexp.Atom a) = serialize a

  deserialize sexp@(Sexp.Cons x xs) =
    case deserialize sexp :: Serialize a => Maybe a of
      Just ps -> Just (Atom (P ps Nothing))
      Nothing -> Sexp.Cons <$> deserialize x <*> deserialize xs
  deserialize Sexp.Nil = Just Sexp.Nil
  deserialize (Atom i) =
    Atom
      <$>
      -- why can't I dispatch to the atom call?
      case i of
        A n i -> Just $ A n i
        N n i -> Just $ N n i
        D n i -> Just $ D n i
        S n i -> Just $ S n i
        P () _ -> Nothing

instance Serialize a => Serialize (Atom a) where
  serialize i =
    case i of
      A n i -> Atom $ A n i
      N n i -> Atom $ N n i
      D n i -> Atom $ D n i
      S n i -> Atom $ S n i
      P n _ -> serialize n
  deserialize (Atom i) =
    case i of
      A n i -> Just $ A n i
      N n i -> Just $ N n i
      D n i -> Just $ D n i
      S n i -> Just $ S n i
      P () _ -> Nothing
  deserialize _ = Nothing

instance Serialize a => Serialize [a] where
  serialize xs = foldr' (Cons . serialize) Sexp.Nil xs
  deserialize c@Cons {} =
    foldr (\x xs -> flip (:) <$> xs <*> deserialize x) (Just []) c
  deserialize Nil = Just []
  deserialize _ = Nothing

instance (Eq a, Hashable a, Serialize a) => Serialize (Set.HashSet a) where
  serialize = serialize . Set.toList
  deserialize = fmap Set.fromList . deserialize

instance Serialize Text where
  serialize i = Atom (S i Nothing)
  deserialize (Atom (S i Nothing)) = Just i
  deserialize _ = Nothing

instance Serialize Double where
  serialize i = Atom (D i Nothing)
  deserialize (Atom (D i Nothing)) = Just i
  deserialize _ = Nothing

instance Serialize Symbol where
  serialize i = Atom (A (NameSymbol.fromSym i) Nothing)
  deserialize (Atom (A i Nothing)) = Just (NameSymbol.toSym i)
  deserialize _ = Nothing

instance Serialize NameSymbol.T where
  serialize i = Atom (A i Nothing)
  deserialize (Atom (A i Nothing)) = Just i
  deserialize _ = Nothing

instance Serialize Integer where
  serialize i = Atom (N i Nothing)
  deserialize (Atom (N i Nothing)) = Just i
  deserialize _ = Nothing

instance Serialize Natural where
  serialize i = Atom (N (toInteger i) Nothing)
  deserialize (Atom (N i Nothing)) = Just $ fromInteger i
  deserialize _ = Nothing

instance Serialize Int where
  serialize i = serialize (toInteger i)
  deserialize s = fmap fromIntegral (deserialize @Integer s)

instance Serialize () where
  serialize () = Nil
  deserialize Nil = Just ()
  deserialize _ = Nothing

----------------------------------------
-- Sexp helper functions
----------------------------------------

foldr :: (Base a -> p -> p) -> p -> Base a -> p
foldr f acc ts =
  case ts of
    Cons a as -> f a (foldr f acc as)
    Atom ____ -> f ts acc
    Nil -> acc

append :: Base a -> Base a -> Base a
append xs ys = foldr Cons ys xs

-- | @car@ grabs the head of the list
car :: Base a -> Base a
car (Cons x _) = x
car Nil = Nil
car (Atom a) = Atom a

-- | @cdr@ grabs the tail of the list
cdr :: Base a -> Base a
cdr (Cons _ xs) = xs
cdr Nil = Nil
cdr (Atom a) = Atom a
