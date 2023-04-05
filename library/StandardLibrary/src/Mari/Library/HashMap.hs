-- |
-- - The HashMap for the codebase.
-- - Basically just imports Data.HashMap.Strict
--   + While giving the operation =!?=.
-- - Every hash in the code base should use this, except when it needs
--   to compare keys by the =Ordering= metric instead.
module Mari.Library.HashMap
  ( module Data.HashMap.Strict,
    Map,
    T,
  )
where

import Data.HashMap.Strict

type Map = HashMap

type T = Map
