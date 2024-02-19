module Juvix.Compiler.Reg.Extra.Recursors where

import Data.Functor.Identity
import Juvix.Compiler.Reg.Language

data ForwardRecursorSig m c = ForwardRecursorSig
  { _forwardFun :: Instruction -> c -> m (c, Instruction),
    _forwardCombine :: NonEmpty c -> c
  }

data BackwardRecursorSig m a = BackwardRecursorSig
  { _backwardFun :: Code -> a -> [a] -> m (a, Code),
    _backwardAdjust :: a -> a
  }

makeLenses ''ForwardRecursorSig
makeLenses ''BackwardRecursorSig

recurseF :: forall m c. (Monad m) => ForwardRecursorSig m c -> c -> Code -> m (c, Code)
recurseF sig c = \case
  i : instrs -> do
    (c0, i0) <- (sig ^. forwardFun) i c
    (c', i') <-
      case i0 of
        Branch x@InstrBranch {..} -> do
          (c1, is1) <- recurseF sig c0 _instrBranchTrue
          (c2, is2) <- recurseF sig c0 _instrBranchFalse
          let c' = (sig ^. forwardCombine) (c1 :| [c2])
          return (c', Branch x {_instrBranchTrue = is1, _instrBranchFalse = is2})
        Case x@InstrCase {..} -> do
          brs' <- mapM goBranch _instrCaseBranches
          def' <- maybe (return Nothing) (\is -> Just <$> recurseF sig c0 is) _instrCaseDefault
          let cs = map fst brs' ++ maybe [] (\md -> [fst md]) def'
              brs = map snd brs'
              def = maybe Nothing (Just . snd) def'
              c' = (sig ^. forwardCombine) (nonEmpty' cs)
          return (c', Case x {_instrCaseBranches = brs, _instrCaseDefault = def})
          where
            goBranch :: CaseBranch -> m (c, CaseBranch)
            goBranch br@CaseBranch {..} = do
              (c', is') <- recurseF sig c0 _caseBranchCode
              return (c', br {_caseBranchCode = is'})
        Block x@InstrBlock {..} -> do
          (c', is) <- recurseF sig c0 _instrBlockCode
          return (c', Block x {_instrBlockCode = is})
        _ ->
          return (c0, i0)
    (c'', instrs') <- recurseF sig c' instrs
    return (c'', i' : instrs')
  [] ->
    return (c, [])

recurseB :: forall m a. (Monad m) => BackwardRecursorSig m a -> a -> Code -> m (a, Code)
recurseB sig a = \case
  i : instrs -> do
    (a', instrs') <- recurseB sig a instrs
    let a0 = (sig ^. backwardAdjust) a'
    (as, i') <-
      case i of
        Branch x@InstrBranch {..} -> do
          (a1, is1) <- recurseB sig a0 _instrBranchTrue
          (a2, is2) <- recurseB sig a0 _instrBranchFalse
          return ([a1, a2], Branch x {_instrBranchTrue = is1, _instrBranchFalse = is2})
        Case x@InstrCase {..} -> do
          brs' <- mapM goBranch _instrCaseBranches
          def' <- maybe (return Nothing) (\is -> Just <$> recurseB sig a0 is) _instrCaseDefault
          let as = map fst brs' ++ maybe [] (\md -> [fst md]) def'
              brs = map snd brs'
              def = maybe Nothing (Just . snd) def'
          return (as, Case x {_instrCaseBranches = brs, _instrCaseDefault = def})
          where
            goBranch :: CaseBranch -> m (a, CaseBranch)
            goBranch br@CaseBranch {..} = do
              (aa, is') <- recurseB sig a0 _caseBranchCode
              return (aa, br {_caseBranchCode = is'})
        Block x@InstrBlock {..} -> do
          (aa, is) <- recurseB sig a0 _instrBlockCode
          return ([aa], Block x {_instrBlockCode = is})
        _ ->
          return ([], i)
    (sig ^. backwardFun) (i' : instrs') a' as
  [] ->
    (sig ^. backwardFun) [] a []

cmapM :: (Monad m) => (Code -> m Code) -> Code -> m Code
cmapM f is0 = do
  ((), is) <-
    recurseB
      BackwardRecursorSig
        { _backwardFun = \is _ _ -> do
            is' <- f is
            return ((), is'),
          _backwardAdjust = id
        }
      ()
      is0
  return is

cmap :: (Code -> Code) -> Code -> Code
cmap f is = runIdentity (cmapM (return . f) is)

imapM :: (Monad m) => (Instruction -> m Instruction) -> Code -> m Code
imapM f = cmapM $ \case
  i : is -> do
    i' <- f i
    return (i' : is)
  [] ->
    return []

imap :: (Instruction -> Instruction) -> Code -> Code
imap f is = runIdentity (imapM (return . f) is)

ifoldFM :: (Monad m, Monoid a) => (a -> Instruction -> m a) -> a -> Code -> m a
ifoldFM f a0 is0 =
  fst
    <$> recurseF
      ForwardRecursorSig
        { _forwardFun = \i a -> do
            a' <- f a i
            return (a', i),
          _forwardCombine = mconcat . toList
        }
      a0
      is0

ifoldF :: (Monoid a) => (a -> Instruction -> a) -> a -> Code -> a
ifoldF f a is = runIdentity (ifoldFM (\a' -> return . f a') a is)
