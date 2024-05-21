module Juvix.Compiler.Backend.Rust.Language where

import Juvix.Prelude

data Type
  = Word
  | VecOfWord
  | Memory

data FunctionArgument = FunctionArgument
  { _functionArgumentType :: Type,
    _functionArgumentName :: Text,
    _functionArgumentMutable :: Bool
  }

data Function = Function
  { _functionArguments :: [FunctionArgument],
    _functionReturnType :: Maybe Type,
    _functionBody :: [Statement]
  }

data Statement
  = StatementLet Let
  | StatementConst ConstDecl
  | StatementAssignment Assignment
  | StatementMatch Match
  | StatementLoop Loop
  | StatementContinue
  | StatementReturn Return
  | StatementExpression Expression

data Let = Let
  { _letVariable :: Text,
    _letMutable :: Bool,
    _letInitializer :: Maybe Expression
  }

data ConstDecl = ConstDecl
  { _constVariable :: Text,
    _constValue :: Expression
  }

data Assignment = Assignment
  { _assignmentVariable :: Text,
    _assignmentValue :: Expression
  }

data MatchBranch = MatchBranch
  { _matchBranchPattern :: Expression,
    _matchBranchBody :: Expression
  }

data Match = Match
  { _matchValue :: Expression,
    _matchBranches :: [MatchBranch]
  }

data Loop = Loop
  { _loopLabel :: Maybe Text,
    _loopBody :: [Statement]
  }

data Return = Return
  { _returnValue :: Expression
  }

data Expression
  = ExprCall Call
  | ExprVec Vec
  | ExprArray Array
  | ExprLiteral Literal

data Call = Call
  { _callFunction :: Text,
    _callArgs :: [Expression]
  }

data Vec = Vec
  { _vecArgs :: [Expression]
  }

data Array = Array
  { _arrayArgs :: [Expression]
  }

data Literal
  = LitInteger Integer
  | LitString Text

makeLenses ''FunctionArgument
makeLenses ''Function
makeLenses ''Let
makeLenses ''ConstDecl
makeLenses ''Assignment
makeLenses ''MatchBranch
makeLenses ''Match
makeLenses ''Loop
makeLenses ''Return
makeLenses ''Call
makeLenses ''Vec
makeLenses ''Array
