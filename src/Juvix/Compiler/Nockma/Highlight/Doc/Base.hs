module Juvix.Compiler.Nockma.Highlight.Doc.Base
  ( module Juvix.Compiler.Nockma.Highlight.Doc.Base,
    module Juvix.Compiler.Nockma.Language,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Compiler.Nockma.Language (NockOp (..))
import Juvix.Data.Keyword.All
  ( delimBracketL,
    delimBracketR,
    delimParenL,
    delimParenR,
    delimRule,
    delimSemicolon,
    kwAnd,
    kwDoubleArrowR,
    kwIndex,
    kwNockmaLogicAnd,
    kwReplace,
    kwStar,
    kwSucc,
  )
import Juvix.Prelude

mytext :: Text
mytext =
  [__i|
  s * [t1t2] => t' && s * t3 => t''
  ---
  s * [[t1t2]t3] => [t't'']
  and
  ---
  s * [?t] => s!p
  and
  ---
  t'2
  |]

data Symbol = Symbol
  { _symbolLetter :: Char,
    _symbolSubscript :: Maybe Natural,
    _symbolPrimes :: Natural
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable Symbol

data PathSymbol = PathP

data Atom
  = AtomSymbol Symbol
  | AtomOperator NockOp
  | AtomReplace Replace
  | AtomIndex IndexAt
  | AtomStack
  | AtomZero
  | AtomOne
  | AtomSuccessor Successor

-- | Syntax: succ _successor
newtype Successor = Successor
  { _successor :: Term
  }

-- | Syntax: index(_indexAtBase; _indexAtIndex)
data IndexAt = IndexAt
  { _indexAtBase :: Term,
    _indexAtPath :: PathSymbol
  }

-- | Syntax: replace (_replaceBase; _replacePath;  _replaceBy)
data Replace = Replace
  { _replaceBase :: Term,
    _replacePath :: PathSymbol,
    _replaceBy :: Term
  }

data Term
  = TermAtom Atom
  | TermCell Cell

data Cell = Cell
  { _cellLhs :: Term,
    _cellRhs :: Term
  }

-- | Syntax: _evalContext => _evalRhs
data EvalRelation = EvalRelation
  { _evalContext :: Context,
    _evalRhs :: Term
  }

-- | Syntax: _contextLhs * _contextRhs
data Context = Context
  { _contextLhs :: Term,
    _contextRhs :: Term
  }

-- | Syntax:
-- rel_1 && .. && rel_n
-- ---
-- rel
data Rule = Rule
  { _ruleConditions :: [EvalRelation],
    _rulePost :: EvalRelation
  }

-- | Syntax:
-- rule_1
-- and
-- ..
-- and
-- rule_n
newtype Rules = Rules
  { _rules :: NonEmpty Rule
  }
