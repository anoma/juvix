module Juvix.Compiler.Store.Core.Language
  ( module Juvix.Compiler.Store.Core.Language,
    module Juvix.Compiler.Core.Language.Nodes,
  )
where

import Juvix.Compiler.Core.Language.Nodes
import Juvix.Extra.Serialize

{---------------------------------------------------------------------------------}

type Type = Node

type Var = Var' ()

type Ident = Ident' ()

type Constant = Constant' ()

type App = App' () Node

type BuiltinApp = BuiltinApp' () Node

type Constr = Constr' () Node

type Lambda = Lambda' () Node Type

type LetItem = LetItem' Node Type

type Let = Let' () Node Type

type LetRec = LetRec' () Node Type

type Case = Case' () () Node Type

type CaseBranch = CaseBranch' () Node Type

type PiLhs = PiLhs' () Node

type Pi = Pi' () Node

type Univ = Univ' ()

type TypeConstr = TypeConstr' () Node

type TypePrim = TypePrim' ()

type Dynamic = Dynamic' ()

type Bottom = Bottom' () Node

{---------------------------------------------------------------------------------}

data Node
  = NVar Var
  | NIdt Ident
  | NCst Constant
  | NApp App
  | NBlt BuiltinApp
  | NCtr Constr
  | NLam Lambda
  | NLet Let
  | NRec LetRec
  | NCase Case
  | NPi Pi
  | NUniv Univ
  | NTyp TypeConstr
  | NPrim TypePrim
  | NDyn Dynamic
  | NBot Bottom
  deriving stock (Generic)

instance Serialize Node
