{-# LANGUAGE ImpredicativeTypes #-}

module Juvix.Prelude.Lens where

import Juvix.Prelude.Base

-- | Points to the first element of a non-empty list.
_head1 :: Lens' (NonEmpty a) a
_head1 = singular each

_tail1 :: Lens' (NonEmpty a) [a]
_tail1 f (h :| hs) = do
  hs' <- f hs
  pure (h :| hs')

-- | View a non-empty list as the init part plus the last element.
_unsnoc1 :: Lens (NonEmpty a) (NonEmpty b) ([a], a) ([b], b)
_unsnoc1 afb la = uncurryF (|:) (afb (maybe [] toList minit, lasta))
  where
    (minit, lasta) = nonEmptyUnsnoc la

-- | Points to the last element of a non-empty list.
_last1 :: Lens' (NonEmpty a) a
_last1 = _unsnoc1 . _2

_self :: Prism' a a
_self = prism' id Just

overM :: (Applicative m) => Lens' a b -> (b -> m b) -> a -> m a
overM l f a = do
  a' <- f (a ^. l)
  return $ set l a' a

setAndRemember :: LensLike ((,) a) s t a b -> b -> s -> (a, t)
setAndRemember = (<<.~)

-- | Extracts the getter from a prism
prismView :: Prism s t a b -> b -> t
prismView aprism = withPrism aprism const

matchingMaybe :: Prism s s a b -> s -> Maybe a
matchingMaybe pri = either (const Nothing) return . matching pri

-- | Arguments:
--
-- 1. `a` is the object that we traverse.
--
-- 2. `expr` is the expression
--
-- 3. `node` is the particular part that we are interested in and want to
-- collect or modify.
--
-- 4. `subExpr` is something that can be unequivocally transformed into 'expr'.
-- Most of the time `subExpr` == `expr`. See platedTraverseNode'
--
-- FIXME what if 'a' == `node`
platedTraverseNode ::
  forall a expr node subExpr.
  (Plated expr) =>
  (Maybe (Prism' a expr)) ->
  Traversal' a expr ->
  Prism expr expr node subExpr ->
  Traversal a a node subExpr
platedTraverseNode mayPrismSelf childr pri = go
  where
    go :: forall f. (Applicative f) => (node -> f subExpr) -> a -> f a
    go g xtop =
      case mayPrismSelf of
        Nothing -> childr expToExp xtop
        Just prismSelf -> case matchingMaybe prismSelf xtop of
          Nothing -> childr expToExp xtop
          Just (selfExpr :: expr) ->
            let tt :: f expr = expToExp selfExpr
             in atoExpr <$> tt
            where
              atoExpr :: expr -> a
              atoExpr = prismView prismSelf
      where
        toExpr :: subExpr -> expr
        toExpr = prismView pri

        expToExp :: expr -> f expr
        expToExp x =
          case matchingMaybe pri x of
            Just (l :: leaf) -> toExpr <$> g l
            Nothing -> platedTraverseNode Nothing plate pri g x

platedTraverseNode' ::
  forall a expr node.
  (Plated expr) =>
  (Maybe (Prism' a expr)) ->
  Traversal' a expr ->
  (expr -> Maybe node) ->
  Traversal a a node expr
platedTraverseNode' prismSelf childr getnode = platedTraverseNode prismSelf childr (prism' id getnode)
