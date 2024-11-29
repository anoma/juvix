module Juvix.Compiler.Core.Transformation.Optimize.LoopHoisting (loopHoisting) where

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.FreeVarsInfo qualified as Info
import Juvix.Compiler.Core.Info.VolatilityInfo qualified as Info
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.ComputeTypeInfo (computeNodeType')

loopHoisting :: Module -> Module
loopHoisting md = mapT (const (umapL go)) md
  where
    go :: BinderList Binder -> Node -> Node
    go bl node = case node of
      NApp {} -> case h of
        NIdt Ident {..}
          | Just ii <- lookupIdentifierInfo' md _identSymbol,
            length args == ii ^. identifierArgsNum ->
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
          | isHoistable sym argNum ->
              goLamApp bl sym info h arg (argNum + 1) args'
        _ -> goApp bl sym (mkApp info h arg) (argNum + 1) args'

    isHoistable :: Symbol -> Int -> Bool
    isHoistable sym argNum =
      isArgRecursiveInvariant md sym argNum && isDirectlyRecursive md sym

    goLamApp :: BinderList Binder -> Symbol -> Info -> Node -> Node -> Int -> [(Info, Node)] -> Node
    goLamApp bl sym info h arg argNum args'
      | null subterms = goApp bl sym (mkApp info h arg) argNum args'
      | otherwise =
          setLetsVolatile n $
            mkLets'
              (map (\node -> (computeNodeType' md bl node, node)) subterms')
              ( adjustLetBoundVars
                  . shift n
                  $ (mkApps (mkApp info h (reLambdasRev lams body')) args')
              )
      where
        (lams, body) = unfoldLambdasRev arg
        bl' = BL.prepend (map (^. lambdaLhsBinder) lams) bl
        (subterms, body') = extractMaximalInvariantSubterms (length bl) bl' body
        n = length subterms
        subterms' = zipWith shift [0 ..] subterms

    extractMaximalInvariantSubterms :: Int -> BinderList Binder -> Node -> ([Node], Node)
    extractMaximalInvariantSubterms initialBindersNum bl0 body =
      first (map (removeInfo Info.kFreeVarsInfo))
        . second (removeInfo Info.kFreeVarsInfo)
        . run
        . runState []
        $ dmapLRM' (bl0, extract) (Info.computeFreeVarsInfo body)
      where
        extract :: (Member (State [Node]) r) => BinderList Binder -> Node -> Sem r Recur
        extract bl node
          | not (isImmediate md node || isLambda node)
              && isFullyApplied md bl node
              && null boundVars = do
              k <- length <$> get @[Node]
              modify' ((shift (-n) node) :)
              -- This variable is later adjusted to the correct index in `adjustLetBoundVars`
              return $ End (mkVar' (-k - 1))
          | otherwise =
              return $ Recur node
          where
            boundVars = filter (< n) $ Info.getFreeVars node
            n = length bl - initialBindersNum

    adjustLetBoundVars :: Node -> Node
    adjustLetBoundVars = umapN adjust
      where
        adjust :: Level -> Node -> Node
        adjust n node = case node of
          NVar Var {..}
            | _varIndex < 0 -> mkVar' (n - _varIndex - 1)
          _ -> node

    setLetsVolatile :: Int -> Node -> Node
    setLetsVolatile n
      | n == 0 = id
      | otherwise = \case
          NLet Let {..} ->
            NLet
              Let
                { _letInfo = Info.insert (Info.VolatilityInfo True) _letInfo,
                  _letBody = setLetsVolatile (n - 1) _letBody,
                  _letItem
                }
          node -> node
