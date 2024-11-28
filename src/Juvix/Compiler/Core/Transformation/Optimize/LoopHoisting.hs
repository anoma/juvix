module Juvix.Compiler.Core.Transformation.Optimize.LoopHoisting (loopHoisting) where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.FreeVarsInfo qualified as Info
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.ComputeTypeInfo (computeNodeType')

loopHoisting :: Module -> Module
loopHoisting md = mapT (const (umapL go)) md
  where
    go :: BinderList Binder -> Node -> Node
    go bl node = case node of
      NApp {} -> case h of
        NIdt Ident {..} ->
          goApp bl _identSymbol h 0 args
        _ -> node
        where
          (h, args) = unfoldApps node
      _ ->
        node

    goApp :: BinderList Binder -> Symbol -> Node -> Int -> [(Info, Node)] -> Node
    goApp bl sym h argNum args = case args of
      [] -> h
      (info, arg) : args' -> case arg of
        NLam {}
          | isArgRecursiveInvariant md sym argNum && isDirectlyRecursive md sym ->
              goApp bl sym (goLamApp bl info h arg) (argNum + 1) args'
        _ -> goApp bl sym (mkApp info h arg) (argNum + 1) args'

    goLamApp :: BinderList Binder -> Info -> Node -> Node -> Node
    goLamApp bl info h arg
      | null subterms = mkApp info h arg
      | otherwise =
          mkLets'
            (map (\node -> (computeNodeType' md bl node, node)) subterms)
            (mkApp info h (reLambdasRev lams body'))
      where
        (lams, body) = unfoldLambdasRev arg
        (subterms, body') = extractMaximalInvariantSubterms (length lams) body

    extractMaximalInvariantSubterms :: Int -> Node -> ([Node], Node)
    extractMaximalInvariantSubterms bindersNum body =
      first (map (removeInfo Info.kFreeVarsInfo))
        . second (removeInfo Info.kFreeVarsInfo)
        . run
        . runState []
        $ dmapNRM extract (Info.computeFreeVarsInfo body)
      where
        extract :: (Member (State [Node]) r) => Level -> Node -> Sem r Recur
        extract n node
          | not (isImmediate md node || isLambda node)
              && isFullyApplied md node
              && null fvars = do
              k <- length <$> get @[Node]
              modify' (node :)
              return $ End (mkVar' (n + bindersNum + k))
          | otherwise =
              return $ Recur node
          where
            fvars = filter (>= n + bindersNum) $ Info.getFreeVars node
