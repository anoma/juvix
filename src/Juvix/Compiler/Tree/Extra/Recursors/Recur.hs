{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Compiler.Tree.Extra.Recursors.Recur where

import Data.Functor.Identity
import Juvix.Compiler.Core.Extra.Recursors.Classes
import Juvix.Compiler.Tree.Language

data Recur' c
  = End' Node
  | Recur' (c, Node)

data Recur
  = End Node
  | Recur Node

instance EmbedIdentity' (c, Node) where
  embedIden' = Identity

instance EmbedIdentity' Node where
  embedIden' = Identity

instance EmbedIdentity' Recur where
  embedIden' = Identity

instance EmbedIdentity' (Recur' c) where
  embedIden' = Identity
