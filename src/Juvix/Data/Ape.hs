module Juvix.Data.Ape where

import Juvix.Prelude
-- import Juvix.Prelude.Pretty qualified as PP

-- | Abstract pretty expression
data Ape a =
  ApeLeaf (Leaf a)
  | ApeApp (App a)
  | ApeInfix (Infix a)

data Leaf a = Leaf {
  _leafDoc :: Doc a,
  _leafAtomicity :: Atomicity
 }

data App a = App {
  _appLeft :: Ape a,
  _appRight :: Ape a
  }

data Infix a = Infix {
  _infixFixity :: Fixity,
  _infixLeft :: Doc a,
  _infixOp :: Doc a,
  _infixRight :: Doc a
 }

data Postfix a = Postfix {
  _postfixFixity :: Fixity,
  _postfixLeft :: Doc a,
  _postfixOp :: Doc a
 }
