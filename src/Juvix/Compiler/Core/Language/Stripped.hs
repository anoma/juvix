module Juvix.Compiler.Core.Language.Stripped
  ( module Juvix.Compiler.Core.Language.Base,
    module Juvix.Compiler.Core.Language.Nodes,
    module Juvix.Compiler.Core.Language.Stripped.Type,
    module Juvix.Compiler.Core.Language.Stripped,
  )
where

{- Stripped program tree datatype -}

import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Language.Nodes
import Juvix.Compiler.Core.Language.Stripped.Type

{---------------------------------------------------------------------------------}

data VarInfo = VarInfo
  { _varInfoName :: Text,
    _varInfoLocation :: Maybe Location,
    _varInfoType :: Type -- TyDynamic if not available
  }

data IdentInfo = IdentInfo
  { _identInfoName :: Text,
    _identInfoLocation :: Maybe Location,
    _identInfoType :: Type
  }

data ConstrInfo = ConstrInfo
  { _constrInfoName :: Text,
    _constrInfoLocation :: Maybe Location,
    _constrInfoType :: Type
  }

data CaseBranchInfo = CaseBranchInfo
  { _caseBranchInfoConstrName :: Text,
    _caseBranchInfoConstrType :: Type
  }

{---------------------------------------------------------------------------------}

type Var = Var' VarInfo

type Ident = Ident' IdentInfo

type Constant = Constant' ()

type Apps = Apps' () Fun Node

data Fun
  = FunVar Var
  | FunIdent Ident
  deriving stock (Eq)

type BuiltinApp = BuiltinApp' () Node

type Constr = Constr' ConstrInfo Node

type Binder = Binder' Type

type LetItem = LetItem' Node Type

type Let = Let' () Node Type

type Case = Case' () CaseBranchInfo Node Type

type CaseBranch = CaseBranch' CaseBranchInfo Node Type

{---------------------------------------------------------------------------------}

data Node
  = NVar Var
  | NIdt Ident
  | NCst Constant
  | NApp Apps
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

makeLenses ''VarInfo
makeLenses ''IdentInfo
makeLenses ''ConstrInfo
makeLenses ''CaseBranchInfo
