module Juvix.Data.Ape.Base where

import Juvix.Prelude

class IsApe a e where
  toApe :: a -> Ape e

-- TODO add ApeParens

-- | Abstract pretty expression
data Ape a
  = ApeLeaf (Leaf a)
  | ApeInfix (Infix a)
  | ApePostfix (Postfix a)

-- TODO add CapeParens

-- | Abstract pretty expressions with chains
data Cape a
  = CapeLeaf (Leaf a)
  | CapeChain (Chain a)
  | CapeUChain (UChain a)

-- | A binary chain of application with the same fixity
data Chain a = Chain
  { _chainFixity :: Fixity,
    _chainHead :: Cape a,
    _chainLinks :: NonEmpty (Maybe a, Cape a)
  }

-- | A unary chain of application with the same fixity
data UChain a = UChain
  { _uchainFixity :: Fixity,
    _uchainHead :: Cape a,
    _uchainOps :: NonEmpty a
  }

data Leaf a = Leaf
  { _leafExpr :: a,
    _leafAtomicity :: Atomicity
  }

data Infix a = Infix
  { _infixFixity :: Fixity,
    _infixLeft :: Ape a,
    _infixOp :: Maybe a,
    _infixRight :: Ape a
  }

data Postfix a = Postfix
  { _postfixFixity :: Fixity,
    _postfixLeft :: Ape a,
    _postfixOp :: a
  }

makeLenses ''Leaf
makeLenses ''Chain
makeLenses ''UChain
makeLenses ''Infix
makeLenses ''Postfix

toCape :: forall a. Ape a -> Cape a
toCape = \case
  ApeLeaf l -> CapeLeaf l
  ApeInfix a -> unfoldInfix a
  ApePostfix p -> CapeUChain (unfoldPostfix p)
  where
    unfoldPostfix :: Postfix a -> UChain a
    unfoldPostfix (Postfix fx l op) =
      UChain
        { _uchainFixity = fx,
          _uchainHead,
          _uchainOps
        }
      where
        (_uchainHead, _uchainOps) = go (pure op) l
        go :: NonEmpty a -> Ape a -> (Cape a, NonEmpty a)
        go ops = \case
          ApePostfix (Postfix fx' l' op')
            | fx == fx' -> go (pure op' <> ops) l'
          e -> (toCape e, ops)

    unfoldInfix :: Infix a -> Cape a
    unfoldInfix (Infix fx l op r)
      | isLeftAssoc fx = CapeChain leftAssoc
      | isRightAssoc fx = CapeChain rightAssoc
      | otherwise = error "what to do?"
      where
        rightAssoc :: Chain a
        rightAssoc =
          Chain
            { _chainFixity = fx,
              _chainHead = toCape l,
              _chainLinks = go op r
            }
          where
            go :: Maybe a -> Ape a -> NonEmpty (Maybe a, Cape a)
            go prevOp = \case
              ApeInfix (Infix fx' l' op' r')
                | fx == fx' -> pure (prevOp, toCape l') <> go op' r'
              e -> pure (prevOp, toCape e)
        leftAssoc :: Chain a
        leftAssoc =
          Chain
            { _chainFixity = fx,
              _chainHead,
              _chainLinks
            }
          where
            (_chainHead, _chainLinks) = go (pure (op, toCape r)) l
            go :: NonEmpty (Maybe a, Cape a) -> Ape a -> (Cape a, NonEmpty (Maybe a, Cape a))
            go ac = \case
              ApeInfix (Infix fx' l' op' r')
                | fx == fx' -> go (pure (op', toCape r') <> ac) l'
              e -> (toCape e, ac)

instance HasAtomicity (Leaf a) where
  atomicity = (^. leafAtomicity)

instance HasAtomicity (Infix a) where
  atomicity = Aggregate . (^. infixFixity)

instance HasAtomicity (Postfix a) where
  atomicity = Aggregate . (^. postfixFixity)

instance HasAtomicity (Chain a) where
  atomicity = Aggregate . (^. chainFixity)

instance HasAtomicity (UChain a) where
  atomicity = Aggregate . (^. uchainFixity)

instance HasAtomicity (Ape a) where
  atomicity = \case
    ApeLeaf l -> atomicity l
    ApeInfix a -> atomicity a
    ApePostfix p -> atomicity p

instance HasAtomicity (Cape a) where
  atomicity = \case
    CapeLeaf l -> atomicity l
    CapeChain c -> atomicity c
    CapeUChain c -> atomicity c
