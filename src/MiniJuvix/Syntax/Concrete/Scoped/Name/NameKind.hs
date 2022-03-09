module MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind where

import MiniJuvix.Prelude

data NameKind
  = -- | Constructor name.
    KNameConstructor
  | -- | Name introduced by the inductive keyword.
    KNameInductive
  | -- | Name of a defined function (top level or let/where block).
    KNameFunction
  | -- | A locally bound name (patterns, arguments, etc.).
    KNameLocal
  | -- | An axiom.
    KNameAxiom
  | -- | An local module name.
    KNameLocalModule
  | -- | An top module name.
    KNameTopModule
  deriving stock (Show, Eq)

class HasNameKind a where
  getNameKind :: a -> NameKind

instance HasNameKind NameKind where
  getNameKind = id

isExprKind :: HasNameKind a => a -> Bool
isExprKind k = case getNameKind k of
    KNameConstructor -> True
    KNameInductive -> True
    KNameFunction -> True
    KNameLocal -> True
    KNameAxiom -> True
    KNameLocalModule -> False
    KNameTopModule -> False

isModuleKind :: HasNameKind a => a -> Bool
isModuleKind k = case getNameKind k of
  KNameLocalModule -> True
  KNameTopModule -> True
  _ -> False

canHaveFixity :: HasNameKind a => a -> Bool
canHaveFixity k = case getNameKind k of
  KNameConstructor -> True
  KNameInductive -> True
  KNameFunction -> True
  KNameAxiom -> True
  KNameLocal -> False
  KNameLocalModule -> False
  KNameTopModule -> False
