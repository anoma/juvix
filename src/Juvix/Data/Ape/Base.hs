module Juvix.Data.Ape.Base where

import Juvix.Prelude

class IsApe a e where
  toApe :: a -> Ape e

-- TODO add ApeParens

-- | Abstract pretty expression
data Ape a
  = ApeLeaf (Leaf a)
  | ApeInfix (Infix a)
  | ApeApp (App a)
  | ApePostfix (Postfix a)

-- TODO add CapeParens

-- | Abstract pretty expressions with chains
data Cape a
  = CapeLeaf (Leaf a)
  | CapeChain (Chain a)
  | CapeAppChain (AppChain a)
  | CapeUChain (UChain a)

-- | A binary chain of application with the same fixity
data Chain a = Chain
  { _chainFixity :: Fixity,
    _chainHead :: Cape a,
    _chainLinks :: NonEmpty (a, Cape a)
  }

data AppChain a = AppChain
  { _achainHead :: Cape a,
    _achainLinks :: NonEmpty (Cape a)
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

data App a = App
  { _appLeft :: Ape a,
    _appRight :: Ape a
  }

data Infix a = Infix
  { _infixFixity :: Fixity,
    _infixLeft :: Ape a,
    _infixOp :: a,
    _infixRight :: Ape a
  }

data Postfix a = Postfix
  { _postfixFixity :: Fixity,
    _postfixLeft :: Ape a,
    _postfixOp :: a
  }

makeLenses ''Leaf
makeLenses ''Chain
makeLenses ''AppChain
makeLenses ''UChain
makeLenses ''Infix
makeLenses ''Postfix

toCape :: forall a. Ape a -> Cape a
toCape = \case
  ApeLeaf l -> CapeLeaf l
  ApeInfix a -> CapeChain (unfoldInfix a)
  ApeApp a -> CapeAppChain (unfoldApp a)
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

    unfoldApp :: App a -> AppChain a
    unfoldApp (App l r) = go (pure (toCape r)) l
      where
        go :: NonEmpty (Cape a) -> Ape a -> AppChain a
        go ac = \case
          ApeApp (App l' r') -> go (pure (toCape r') <> ac) l'
          e ->
            AppChain
              { _achainHead = toCape e,
                _achainLinks = ac
              }

    unfoldInfix :: Infix a -> Chain a
    unfoldInfix (Infix fx l op r)
      | isLeftAssoc fx = leftAssoc
      | isRightAssoc fx = rightAssoc
      | otherwise = noAssoc
      where
        noAssoc :: Chain a
        noAssoc =
          Chain
            { _chainFixity = fx,
              _chainHead = toCape l,
              _chainLinks = pure (op, toCape r)
            }

        rightAssoc :: Chain a
        rightAssoc =
          Chain
            { _chainFixity = fx,
              _chainHead = toCape l,
              _chainLinks = go op r
            }
          where
            go :: a -> Ape a -> NonEmpty (a, Cape a)
            go prevOp = \case
              ApeInfix (Infix fx' l' op' r')
                | fx == fx' -> pure (prevOp, toCape l') <> go op' r'
              e -> pure (prevOp, toCape e)

        leftAssoc :: Chain a
        leftAssoc = go (pure (op, toCape r)) l
          where
            go :: NonEmpty (a, Cape a) -> Ape a -> Chain a
            go ac = \case
              ApeInfix (Infix fx' l' op' r')
                | fx == fx' -> go (pure (op', toCape r') <> ac) l'
              e ->
                Chain
                  { _chainFixity = fx,
                    _chainHead = toCape e,
                    _chainLinks = ac
                  }

instance HasAtomicity (Leaf a) where
  atomicity = (^. leafAtomicity)

instance HasAtomicity (App a) where
  atomicity = const (Aggregate appFixity)

instance HasAtomicity (Infix a) where
  atomicity = Aggregate . (^. infixFixity)

instance HasAtomicity (Postfix a) where
  atomicity = Aggregate . (^. postfixFixity)

instance HasAtomicity (AppChain a) where
  atomicity = const (Aggregate appFixity)

instance HasAtomicity (Chain a) where
  atomicity = Aggregate . (^. chainFixity)

instance HasAtomicity (UChain a) where
  atomicity = Aggregate . (^. uchainFixity)

instance HasAtomicity (Ape a) where
  atomicity = \case
    ApeLeaf l -> atomicity l
    ApeInfix a -> atomicity a
    ApePostfix p -> atomicity p
    ApeApp a -> atomicity a

instance HasAtomicity (Cape a) where
  atomicity = \case
    CapeLeaf l -> atomicity l
    CapeChain c -> atomicity c
    CapeUChain c -> atomicity c
    CapeAppChain c -> atomicity c
