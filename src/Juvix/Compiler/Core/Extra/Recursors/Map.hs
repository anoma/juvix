{-# LANGUAGE UndecidableInstances #-}

module Juvix.Compiler.Core.Extra.Recursors.Map where

import Data.Functor.Identity
import Data.Kind qualified as GHC
import Data.Singletons.TH
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.Parameters

type DirTy :: Direction -> GHC.Type
type family DirTy d = res | res -> d where
  DirTy 'TopDown = Recur
  DirTy 'BottomUp = Node -- For bottom up maps we never recur on the children

-- | `umapG` maps the nodes bottom-up, i.e., when invoking the mapper function the
-- recursive subnodes have already been mapped
umapG ::
  forall c m.
  Monad m =>
  Collector (Int, [Binder]) c ->
  (c -> Node -> m Node) ->
  Node ->
  m Node
umapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m Node
    go c n =
      let ni = destruct n
       in do
            let h n' = (coll ^. cCollect) (n' ^. childBindersNum, n' ^. childBinders) c
            ns <- mapM (\n' -> go (h n') (n' ^. childNode)) (ni ^. nodeChildren)
            f c (reassembleDetails ni ns)

dmapG ::
  forall c m.
  Monad m =>
  Collector (Int, [Binder]) c ->
  (c -> Node -> m Recur) ->
  Node ->
  m Node
dmapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m Node
    go c n = do
      r <- f c n
      case r of
        End n' -> return n'
        Recur n' ->
          let ni = destruct n'
           in reassembleDetails ni <$> mapM goChild (ni ^. nodeChildren)
      where
        goChild :: NodeChild -> m Node
        goChild ch = go ((coll ^. cCollect) (ch ^. childBindersNum, ch ^. childBinders) c) (ch ^. childNode)

type CtxTy :: Ctx -> GHC.Type
type family CtxTy x = res | res -> x where
  CtxTy 'CtxBinderList = BinderList Binder
  CtxTy 'CtxBinderNum = Index
  CtxTy 'CtxNone = ()

type RetTy :: (GHC.Type -> GHC.Type) -> Direction -> Monadic -> Ret -> GHC.Type
type family RetTy m dir mon r = res | res -> mon r where
  RetTy m 'TopDown 'Monadic 'RetRecur = m Recur
  RetTy m 'TopDown 'Monadic 'RetSimple = m Node
  RetTy _ 'TopDown 'NonMonadic 'RetRecur = Recur
  RetTy _ 'TopDown 'NonMonadic 'RetSimple = Node
  RetTy m 'BottomUp 'Monadic 'RetSimple = m Node
  RetTy _ 'BottomUp 'NonMonadic 'RetSimple = Node

type BodyTy :: (GHC.Type -> GHC.Type) -> Direction -> Monadic -> Ctx -> Ret -> GHC.Type
type family BodyTy m dir mon x r = res | res -> x r where
  BodyTy m dir 'Monadic 'CtxBinderList r = BinderList Binder -> Node -> RetTy m dir 'Monadic r
  BodyTy m dir 'Monadic 'CtxBinderNum r = Int -> Node -> RetTy m dir 'Monadic r
  BodyTy m dir 'Monadic 'CtxNone r = Node -> RetTy m dir 'Monadic r
  BodyTy _ dir 'NonMonadic 'CtxBinderList r = BinderList Binder -> Node -> RetTy Identity dir 'NonMonadic r
  BodyTy _ dir 'NonMonadic 'CtxBinderNum r = Int -> Node -> RetTy Identity dir 'NonMonadic r
  BodyTy _ dir 'NonMonadic 'CtxNone r = Node -> RetTy Identity dir 'NonMonadic r

type NodeMapArg :: (GHC.Type -> GHC.Type) -> Direction -> Monadic -> CollectorIni -> Ctx -> Ret -> GHC.Type
type family NodeMapArg m dir mon i x r = res | res -> i x r where
  NodeMapArg m dir mon 'Ini x r = (CtxTy x, BodyTy m dir mon x r)
  NodeMapArg m dir mon 'NoIni 'CtxBinderList r = BinderList Binder -> Node -> RetTy m dir mon r
  NodeMapArg m dir mon 'NoIni 'CtxBinderNum r = Int -> Node -> RetTy m dir mon r
  NodeMapArg m dir mon 'NoIni 'CtxNone r = Node -> RetTy m dir mon r

type NodeMapRet :: (GHC.Type -> GHC.Type) -> Monadic -> GHC.Type
type family NodeMapRet m mon = res | res -> mon m where
  NodeMapRet m 'Monadic = m Node
  NodeMapRet Identity 'NonMonadic = Node

type OverIdentity :: GHC.Type -> GHC.Type
type family OverIdentity t = res where
  OverIdentity (a -> b) = a -> OverIdentity b
  OverIdentity ((), b) = ((), OverIdentity b)
  OverIdentity (BinderList Binder, b) = (BinderList Binder, OverIdentity b)
  OverIdentity (Index, b) = (Index, OverIdentity b)
  OverIdentity leaf = Identity leaf

class EmbedIdentity a where
  embedIden :: a -> OverIdentity a

instance EmbedIdentity b => EmbedIdentity (a -> b) where
  embedIden f = embedIden . f

instance EmbedIdentity b => EmbedIdentity ((), b) where
  embedIden (a, b) = (a, embedIden b)

instance EmbedIdentity b => EmbedIdentity (Index, b) where
  embedIden (a, b) = (a, embedIden b)

instance EmbedIdentity b => EmbedIdentity (BinderList Binder, b) where
  embedIden (a, b) = (a, embedIden b)

instance EmbedIdentity Node where
  embedIden = Identity

instance EmbedIdentity Recur where
  embedIden = Identity

type KDefaultIdentity :: Monadic -> (GHC.Type -> GHC.Type) -> GHC.Constraint
type family KDefaultIdentity mon m = res | res -> m where
  KDefaultIdentity 'Monadic m = m ~ m
  KDefaultIdentity 'NonMonadic m = Identity ~ m

type KCompatibleDir :: Direction -> Ret -> GHC.Constraint
type family KCompatibleDir dir r = res where
  KCompatibleDir 'TopDown r = r ~ r
  KCompatibleDir 'BottomUp r = r ~ 'RetSimple

nodeMapE ::
  forall (dir :: Direction) (mon :: Monadic) (i :: CollectorIni) (x :: Ctx) (r :: Ret) m.
  ( Monad m,
    KDefaultIdentity mon m,
    KCompatibleDir dir r
  ) =>
  Sing dir ->
  Sing mon ->
  Sing i ->
  Sing x ->
  Sing r ->
  NodeMapArg m dir mon i x r ->
  Node ->
  NodeMapRet m mon
nodeMapE sdir smon sini sctx sret f = case smon :: SMonadic mon of
  SMonadic ->
    case (sdir, sini, sctx, sret) of
      (STopDown, SNoIni, SCtxBinderList, SRetSimple) -> nodeMapG' binderInfoCollector (\bi -> fromSimple . f bi)
      (STopDown, SNoIni, SCtxBinderList, SRetRecur) -> nodeMapG' binderInfoCollector f
      (STopDown, SNoIni, SCtxBinderNum, SRetSimple) -> nodeMapG' binderNumCollector (\bi -> fromSimple . f bi)
      (STopDown, SNoIni, SCtxBinderNum, SRetRecur) -> nodeMapG' binderNumCollector f
      (STopDown, SNoIni, SCtxNone, SRetSimple) -> nodeMapG' binderInfoCollector (const (fromSimple . f))
      (STopDown, SNoIni, SCtxNone, SRetRecur) -> nodeMapG' binderInfoCollector (const f)
      (STopDown, SIni, SCtxBinderList, SRetSimple) -> nodeMapG' (binderInfoCollector' (fst f)) (\bi -> fromSimple . snd f bi)
      (STopDown, SIni, SCtxBinderList, SRetRecur) -> nodeMapG' (binderInfoCollector' (fst f)) (snd f)
      (STopDown, SIni, SCtxBinderNum, SRetSimple) -> nodeMapG' (binderNumCollector' (fst f)) (\bi -> fromSimple . snd f bi)
      (STopDown, SIni, SCtxBinderNum, SRetRecur) -> nodeMapG' (binderNumCollector' (fst f)) (snd f)
      (STopDown, SIni, SCtxNone, SRetSimple) -> nodeMapG' binderInfoCollector (const (fromSimple . snd f))
      (STopDown, SIni, SCtxNone, SRetRecur) -> nodeMapG' binderInfoCollector (const (snd f))
      (SBottomUp, SNoIni, SCtxBinderList, SRetSimple) -> nodeMapG' binderInfoCollector f
      (SBottomUp, SNoIni, SCtxBinderNum, SRetSimple) -> nodeMapG' binderNumCollector f
      (SBottomUp, SNoIni, SCtxNone, SRetSimple) -> nodeMapG' binderInfoCollector (const f)
      (SBottomUp, SIni, SCtxBinderList, SRetSimple) -> nodeMapG' (binderInfoCollector' (fst f)) (snd f)
      (SBottomUp, SIni, SCtxBinderNum, SRetSimple) -> nodeMapG' (binderNumCollector' (fst f)) (snd f)
      (SBottomUp, SIni, SCtxNone, SRetSimple) -> nodeMapG' binderInfoCollector (const (snd f))
  SNonMonadic ->
    runIdentity
      . case (sdir, sini, sctx, sret) of
        (STopDown, SNoIni, SCtxBinderList, SRetSimple) -> nodeMapG' binderInfoCollector (\bi -> fromSimple . embedIden f bi)
        (STopDown, SNoIni, SCtxBinderList, SRetRecur) -> nodeMapG' binderInfoCollector (embedIden f)
        (STopDown, SNoIni, SCtxBinderNum, SRetSimple) -> nodeMapG' binderNumCollector (\bi -> fromSimple . embedIden f bi)
        (STopDown, SNoIni, SCtxBinderNum, SRetRecur) -> nodeMapG' binderNumCollector (embedIden f)
        (STopDown, SNoIni, SCtxNone, SRetSimple) -> nodeMapG' binderInfoCollector (const (fromSimple . embedIden f))
        (STopDown, SNoIni, SCtxNone, SRetRecur) -> nodeMapG' binderInfoCollector (const (embedIden f))
        (STopDown, SIni, SCtxBinderList, SRetSimple) -> nodeMapG' (binderInfoCollector' (fst f)) (\bi -> fromSimple . snd (embedIden f) bi)
        (STopDown, SIni, SCtxBinderList, SRetRecur) -> nodeMapG' (binderInfoCollector' (fst f)) (snd (embedIden f))
        (STopDown, SIni, SCtxBinderNum, SRetSimple) -> nodeMapG' (binderNumCollector' (fst f)) (\bi -> fromSimple . snd (embedIden f) bi)
        (STopDown, SIni, SCtxBinderNum, SRetRecur) -> nodeMapG' (binderNumCollector' (fst f)) (snd (embedIden f))
        (STopDown, SIni, SCtxNone, SRetSimple) -> nodeMapG' binderInfoCollector (const (fromSimple . snd (embedIden f)))
        (STopDown, SIni, SCtxNone, SRetRecur) -> nodeMapG' binderInfoCollector (const (snd (embedIden f)))
        (SBottomUp, SNoIni, SCtxBinderList, SRetSimple) -> nodeMapG' binderInfoCollector (embedIden f)
        (SBottomUp, SNoIni, SCtxBinderNum, SRetSimple) -> nodeMapG' binderNumCollector (embedIden f)
        (SBottomUp, SNoIni, SCtxNone, SRetSimple) -> nodeMapG' binderInfoCollector (const (embedIden f))
        (SBottomUp, SIni, SCtxBinderList, SRetSimple) -> nodeMapG' (binderInfoCollector' (fst f)) (snd (embedIden f))
        (SBottomUp, SIni, SCtxBinderNum, SRetSimple) -> nodeMapG' (binderNumCollector' (fst f)) (snd (embedIden f))
        (SBottomUp, SIni, SCtxNone, SRetSimple) -> nodeMapG' binderInfoCollector (const (snd (embedIden f)))
  where
    fromSimple :: forall g. Functor g => g Node -> g Recur
    fromSimple = fmap Recur
    nodeMapG' ::
      Collector (Int, [Binder]) c ->
      (c -> Node -> m (DirTy dir)) ->
      Node ->
      m Node
    nodeMapG' = case sdir of
      STopDown -> dmapG
      SBottomUp -> umapG

-- | Implicit version of dmapE
nodeMapI ::
  forall (dir :: Direction) (mon :: Monadic) (i :: CollectorIni) (x :: Ctx) (r :: Ret) m.
  ( SingI mon,
    SingI i,
    SingI x,
    SingI r,
    Monad m,
    KDefaultIdentity mon m,
    KCompatibleDir dir r
  ) =>
  Sing dir ->
  NodeMapArg m dir mon i x r ->
  Node ->
  NodeMapRet m mon
nodeMapI dir = nodeMapE dir sing sing sing sing
