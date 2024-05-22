module Juvix.Compiler.Backend.Rust.Language where

import Juvix.Prelude

data Type
  = Word
  | VecOfWord
  | Memory

data IsMut
  = Mut
  | NotMut

data FunctionArgument = FunctionArgument
  { _functionArgumentType :: Type,
    _functionArgumentName :: Text,
    _functionArgumentMutable :: IsMut
  }

data Function = Function
  { _functionName :: Text,
    _functionArguments :: [FunctionArgument],
    _functionReturnType :: Maybe Type,
    _functionBody :: [Statement],
    _functionAttributes :: Maybe Text
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
    _letMutable :: IsMut,
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
    _matchBranchBody :: [Statement]
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
