module Juvix.Compiler.Nockma.Highlight.Doc.Base
  ( module Juvix.Compiler.Nockma.Highlight.Doc.Base,
    module Juvix.Compiler.Nockma.Language,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Compiler.Nockma.Language (NockOp (..))
import Juvix.Data.Keyword.All
  ( delimBraceL,
    delimBraceR,
    delimBracketL,
    delimBracketR,
    delimParenL,
    delimParenR,
    delimRule,
    delimSemicolon,
    kwAnd,
    kwDoubleArrowR,
    kwExclamation,
    kwIndex,
    kwMapsTo,
    kwNockmaLogicAnd,
    kwReplace,
    kwStar,
    kwSucc,
  )
import Juvix.Prelude

data Symbol = Symbol
  { _symbolLetter :: Char,
    _symbolSubscript :: Maybe Natural,
    _symbolPrimes :: Natural
  }
  deriving stock (Eq, Ord, Generic, Lift)

instance Hashable Symbol

data PathSymbol = PathP
  deriving stock (Lift)

data Atom
  = AtomSymbol Symbol
  | AtomOperator NockOp
  | AtomReplace Replace
  | AtomIndex IndexAt
  | AtomStack
  | AtomZero
  | AtomOne
  | AtomSuccessor Successor
  deriving stock (Lift)

-- | Syntax: succ(_successor)
newtype Successor = Successor
  { _successor :: Term
  }
  deriving stock (Lift)

-- | Syntax: index(_indexAtBase; _indexAtIndex)
data IndexAt = IndexAt
  { _indexAtBase :: Term,
    _indexAtPath :: PathSymbol
  }
  deriving stock (Lift)

-- | Syntax: replace (_replaceBase; _replacePath;  _replaceBy)
data Replace = Replace
  { _replaceBase :: Term,
    _replacePath :: PathSymbol,
    _replaceBy :: Term
  }
  deriving stock (Lift)

data Term
  = TermAtom Atom
  | TermCell Cell
  deriving stock (Lift)

-- | Syntax: [l r]
data Cell = Cell
  { _cellLhs :: Term,
    _cellRhs :: Term
  }
  deriving stock (Lift)

-- | Syntax: _evalContext => _evalRhs
data EvalRelation = EvalRelation
  { _evalContext :: Context,
    _evalRhs :: Term
  }
  deriving stock (Lift)

-- | Syntax: _contextLhs * _contextRhs
data Context = Context
  { _contextLhs :: Term,
    _contextRhs :: Term
  }
  deriving stock (Lift)

-- | Syntax:
-- rel_1 && .. && rel_n
-- ---
-- rel
data Rule = Rule
  { _ruleConditions :: [EvalRelation],
    _rulePost :: EvalRelation
  }
  deriving stock (Lift)

-- | Syntax:
-- rule_1
-- and
-- ..
-- and
-- rule_n
newtype Rules = Rules
  { _rules :: NonEmpty Rule
  }
  deriving stock (Lift)
