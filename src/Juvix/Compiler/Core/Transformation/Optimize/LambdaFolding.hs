-- An optimizing transformation that converts beta-redexes into let-expressions.
--
-- For example, transforms
-- ```
-- (\x \y x + y * x) a b
-- ```
-- to
-- ```
-- let x := a in let y := b in x + y * x
-- ```
module Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Node -> Node
convertNode = rmap go
  where
    go :: ([BinderChange] -> Node -> Node) -> Node -> Node
    go recur node = case node of
      NApp {} ->
        let (h, args) = unfoldApps' node
            (lams, body) = unfoldLambdas h
         in goLams [] lams args body
        where
          goLams :: [BinderChange] -> [LambdaLhs] -> [Node] -> Node -> Node
          goLams bcs lams args body =
            case (lams, args) of
              ([], _) ->
                mkApps'
                  (go (recur . (revAppend bcs)) body)
                  (map (go (recur . (BCAdd (length bcs) :))) args)
              (lam : lams', arg : args') ->
                mkLet mempty bd' (go (recur . (BCAdd (length bcs) :)) arg) $
                  goLams (BCKeep bd : bcs) lams' args' body
                where
                  bd = lam ^. lambdaLhsBinder
                  bd' = over binderType (go (recur . (revAppend bcs))) bd
              (_, []) ->
                go (recur . (revAppend bcs)) (reLambdas lams body)
      _ ->
        recur [] node

lambdaFolding :: InfoTable -> InfoTable
lambdaFolding = mapAllNodes convertNode
