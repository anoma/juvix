module Juvix.Compiler.Core.Transformation.TopEtaExpand where

import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

topEtaExpand :: Module -> Module
topEtaExpand md = run (mapT' go md)
  where
    go :: Symbol -> Node -> Sem '[InfoTableBuilder] Node
    go sym body = case lookupIdentifierInfo' md sym of
      Nothing -> return body
      Just idenInfo ->
        let args :: [PiLhs]
            args = fst (unfoldPi (idenInfo ^. identifierType))
         in skipLambdas args body
      where
        skipLambdas :: [PiLhs] -> Node -> Sem '[InfoTableBuilder] Node
        skipLambdas args node = case args of
          [] -> return node
          (_ : as) -> case node of
            NLam l -> NLam <$> traverseOf lambdaBody (skipLambdas as) l
            _ -> do
              let binders = map (^. piLhsBinder) args
              overIdentArgs sym (++ binders)
              return (expand node args)
        expand :: Node -> [PiLhs] -> Node
        expand n lhs = reLambdas (map lambdaFromPi lhs) body'
          where
            len = length lhs
            body' = mkApps' (shift "topet" len n) [mkVar' v | v <- reverse [0 .. len - 1]]
        -- We keep the name and type. We drop the other info
        lambdaFromPi :: PiLhs -> LambdaLhs
        lambdaFromPi pi = LambdaLhs mempty (pi ^. piLhsBinder)
