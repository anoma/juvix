{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Sexp.Types where

import Control.Lens hiding (List, from, (:>), (|>))
import qualified Data.Aeson as A
import qualified Data.Fix as Fix
import qualified Data.Functor.Foldable as Foldable
import qualified Data.Functor.Foldable.TH as FunctorTemplates
import Data.Hashable ()
import Mari.Library hiding (show, toList)
import qualified Mari.Library.LineNum as LineNum
import qualified Mari.Library.NameSymbol as NameSymbol
import Prelude (Show (..), String)

type T = Base ()

-- TODO ∷ make Atom generic, and have it conform to an interface?
-- This way we can erase information later!
data Base a
  = Atom (Atom a)
  | Cons {tCar :: Base a, tCdr :: Base a}
  | Nil
  deriving
    ( Eq,
      Ord,
      Generic,
      Typeable,
      Data,
      NFData,
      Read,
      Functor,
      Foldable,
      Traversable
    )

instance (A.ToJSON a) => A.ToJSON (Base a) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON a) => A.FromJSON (Base a) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

deriving anyclass instance (A.ToJSON a) => A.ToJSONKey (Base a)

deriving anyclass instance (A.FromJSON a) => A.FromJSONKey (Base a)

data Atom a
  = A {atomName :: NameSymbol.T, atomLineNum :: Maybe LineNum.T}
  | N {atomNum :: Integer, atomLineNum :: Maybe LineNum.T}
  | D {atomDouble :: Double, atomLineNum :: Maybe LineNum.T}
  | S {atomText :: Text, atomLineNum :: Maybe LineNum.T}
  | P {atomTerm :: a, atomLineNum :: Maybe LineNum.T}
  deriving
    ( Generic,
      Typeable,
      Data,
      NFData,
      Read,
      Show,
      Functor,
      Foldable,
      Traversable
    )

instance A.ToJSON a => A.ToJSON (Atom a) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON a => A.FromJSON (Atom a) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

deriving anyclass instance (A.ToJSON a) => A.ToJSONKey (Atom a)

deriving anyclass instance (A.FromJSON a) => A.FromJSONKey (Atom a)

noLoc :: Atom a -> Atom a
noLoc a = a {atomLineNum = Nothing}

instance Eq a => Eq (Atom a) where
  (==) = (==) `on` from . noLoc

instance Ord a => Ord (Atom a) where
  compare = compare `on` from . noLoc

instance Hashable a => Hashable (Atom a) where
  hash (D {atomDouble}) = hash ('D', atomDouble)
  hash (P {atomTerm}) = hash ('P', atomTerm)
  hash (S {atomText}) = hash ('S', atomText)
  hash (A {atomName}) = hash ('A', atomName)
  hash (N {atomNum}) = hash ('N', atomNum)

instance (Hashable a) => Hashable (Base a) where
  hash (Atom atom) = hash atom
  hash Nil = 1
  hash (Cons {tCar, tCdr}) = hash (hash tCar, hash tCdr)

makeLensesWith camelCaseFields ''Atom

toList' :: Base a -> ([Base a], Maybe (Atom a))
toList' (Cons x xs) = first (x :) $ toList' xs
toList' Nil = ([], Nothing)
toList' (Atom a) = ([], Just a)

toList :: Alternative f => T -> f [T]
toList s = case toList' s of (xs, Nothing) -> pure xs; _ -> empty

infixr 5 :>

pattern (:>) :: Base a -> Base a -> Base a
pattern x :> xs = Cons x xs

{-# COMPLETE (:>), Atom, Nil #-}

pattern List :: [Base a] -> Base a
pattern List xs <-
  (toList' -> (xs, Nothing))
  where
    List xs = foldr Cons Nil xs

pattern IList :: [Base a] -> Atom a -> Base a
pattern IList xs a <-
  (toList' -> (xs, Just a))
  where
    IList xs a = foldr Cons (Atom a) xs

{-# COMPLETE List, IList, Atom #-}

-- TODO ∷ make reader instance

-- TODO ∷ this is poorly written, please simplify

instance Show a => Show (Base a) where
  show (Cons car (Atom a)) =
    "(" <> show car <> " . " <> show (Atom a) <> ")"
  show (Cons car cdr)
    | take 1 (showNoParens cdr) == ")" =
      "(" <> show car <> showNoParens cdr
    | otherwise =
      "(" <> show car <> " " <> showNoParens cdr
  show (Atom (A x _)) =
    show (NameSymbol.toSymbol x)
  show (Atom (N x _)) =
    show x
  show (Atom (D x _)) =
    show x
  show (Atom (S x _)) =
    show x
  show (Atom (P x _)) =
    "#S(" <> show x <> ")"
  show Nil = "()"

showNoParens :: Show a => Base a -> String
showNoParens (Cons car (Atom a)) =
  show car <> " . " <> show (Atom a) <> ")"
showNoParens (Cons car cdr)
  | showNoParens cdr == ")" =
    show car <> showNoParens cdr
  | otherwise =
    show car <> " " <> showNoParens cdr
showNoParens Nil = ")"
showNoParens xs = show xs

---------------------------
---- Recursion schemes ----
---------------------------

-- Define fixpoint versions of Sexp.Base, which provide instances of
-- Recursive and Corecursive, and thereby allow the use of recursion-schemes.

FunctorTemplates.makeBaseFunctor ''Base

type instance Foldable.Base (Base a) = BaseF a

-- Define explicit general fixpoints, least fixpoints, and greatest fixpoints
-- of BaseF (they are all isomorphic to each other and to Base).
type FixBase a = Fix.Fix (BaseF a)

type MuBase a = Fix.Mu (BaseF a)

type NuBase a = Fix.Nu (BaseF a)
