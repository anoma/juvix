module Juvix.Compiler.Core.Language.Stripped where

import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Language.Nodes

{---------------------------------------------------------------------------------}
{- Stripped program tree datatype -}

type instance FVar 'Stripped = Var' Info
type instance FIdent 'Stripped = Ident' Info
type instance FConstant 'Stripped = Constant' Info
type instance FApp 'Stripped = App' Info Node
type instance FBuiltinApp 'Stripped = BuiltinApp' Info Node
type instance FConstr 'Stripped = Constr' Info Node
type instance FLet 'Stripped = Let' Info Node
type instance FCase 'Stripped = Case' Info Node

type Var = FVar 'Stripped
type Ident = FIdent 'Stripped
type Constant = FConstant 'Stripped
type App = FApp 'Stripped
type BuiltinApp = FBuiltinApp 'Stripped
type Constr = FConstr 'Stripped
type Let = FLet 'Stripped
type Case = FCase 'Stripped

data Node
  = NVar Var
  | NIdt Ident
  | NCst Constant
  | NApp App
  | NBlt BuiltinApp
  | NCtr Constr
  | NLet Let
  | NCase Case
  deriving stock (Eq)

instance HasAtomicity Node where
  atomicity = \case
    NVar x -> atomicity x
    NIdt x -> atomicity x
    NCst x -> atomicity x
    NApp x -> atomicity x
    NBlt x -> atomicity x
    NCtr x -> atomicity x
    NLet x -> atomicity x
    NCase x -> atomicity x
