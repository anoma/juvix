module Juvix.Compiler.Backend.Rust.Language where

import Juvix.Prelude

data Program = Program
  { _programFunctions :: [Function]
  }

data Type
  = Word
  | VecOfWord
  | Memory

data IsMut
  = Mut
  | NotMut

data FunctionArgument = FunctionArgument
  { _functionArgumentMutable :: IsMut,
    _functionArgumentName :: Text,
    _functionArgumentType :: Type
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
  | StatementIf If
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

data If = If
  { _ifValue :: Expression,
    _ifBranchTrue :: [Statement],
    _ifBranchFalse :: [Statement]
  }

data MatchBranch = MatchBranch
  { -- Nothing indicates the wildcard
    _matchBranchPattern :: Maybe Expression,
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
  { _returnValue :: Maybe Expression
  }

data Expression
  = ExprVar Var
  | ExprCall Call
  | ExprVec Vec
  | ExprArray Array
  | ExprLiteral Literal
  | ExprBlock Block
  | ExprVerbatim Text

data Var = Var
  { _varName :: Text
  }

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

data Block = Block
  { _blockBody :: [Statement]
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
makeLenses ''Var
makeLenses ''Call
makeLenses ''Vec
makeLenses ''Array
makeLenses ''Block
makeLenses ''Program
