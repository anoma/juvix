module Core.Recursor.RMap where

import Base
import Core.Recursor.Base
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Language

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore recursors rmap tests"
    (map (mkTest . testDescr) tests)

tests :: [UnitTest]
tests =
  [ UnitTest
      "Identity"
      (rmap (\recur -> recur []))
      ( map
          (\x -> (x, x))
          [ mkLambda' mkDynamic' (mkLambda' mkDynamic' (mkVar' 1)),
            mkLambda' mkDynamic' (mkLet' mkDynamic' (mkVar' 0) (mkLambda' mkDynamic' (mkApps' (mkLambdas' [mkDynamic', mkDynamic'] (mkVar' 1)) [mkVar' 1, mkVar' 2]))),
            mkLambdas' [mkTypeInteger', mkTypeInteger'] (mkBuiltinApp' OpIntAdd [mkVar' 1, mkVar' 0])
          ]
      ),
    UnitTest
      "Add lambdas"
      addLambdas
      [ ( mkLambdas' [mkTypeInteger', mkTypeInteger'] (mkBuiltinApp' OpIntAdd [mkVar' 1, mkVar' 0]),
          mkLambdas' [mkDynamic', mkTypeInteger', mkDynamic', mkTypeInteger'] (mkBuiltinApp' OpIntAdd [mkVar' 2, mkVar' 0])
        ),
        ( mkLambdas' [mkTypeInteger', mkTypeInteger'] (mkVar' 1),
          mkLambdas' [mkDynamic', mkTypeInteger', mkDynamic', mkTypeInteger'] (mkVar' 2)
        )
      ],
    UnitTest
      "Replace lambdas with lets"
      replaceLambdasWithLets
      [ ( mkLambdas' [mkTypeInteger', mkTypeInteger'] (mkBuiltinApp' OpIntAdd [mkVar' 1, mkVar' 0]),
          mkLets' [(mkTypeInteger', mkConstant' (ConstInteger 0)), (mkTypeInteger', mkConstant' (ConstInteger 1))] (mkBuiltinApp' OpIntAdd [mkVar' 1, mkVar' 0])
        ),
        ( mkLambda' mkTypeInteger' $ mkLet' mkTypeInteger' (mkVar' 0) $ mkLambda' mkTypeInteger' $ mkBuiltinApp' OpIntAdd [mkVar' 2, mkVar' 0],
          mkLets' [(mkTypeInteger', mkConstant' (ConstInteger 0)), (mkTypeInteger', mkVar' 0), (mkTypeInteger', mkConstant' (ConstInteger 2))] (mkBuiltinApp' OpIntAdd [mkVar' 2, mkVar' 0])
        ),
        ( mkLambda' mkTypeInteger' $ mkVar' 0,
          mkLet' mkTypeInteger' (mkConstant' (ConstInteger 0)) (mkVar' 0)
        )
      ],
    UnitTest
      "Remove lambdas"
      removeLambdas
      [ ( mkLambdas' [mkTypeInteger', mkTypeInteger', mkTypeInteger', mkTypeInteger'] (mkBuiltinApp' OpIntAdd [mkBuiltinApp' OpIntAdd [mkVar' 3, mkVar' 1], mkVar' 2]),
          mkLambdas' [mkTypeInteger', mkTypeInteger'] (mkBuiltinApp' OpIntAdd [mkBuiltinApp' OpIntAdd [mkVar' 1, mkVar' 0], mkVar' 1])
        ),
        ( mkLambdas' [mkTypeInteger', mkTypeInteger'] (mkVar' 0),
          mkLambda' mkTypeInteger' (mkVar' 0)
        ),
        ( mkLambdas' [mkTypeInteger', mkTypeInteger', mkTypeInteger', mkTypeInteger'] (mkVar' 2),
          mkLambdas' [mkTypeInteger', mkTypeInteger'] (mkVar' 1)
        ),
        ( mkLambdas' [mkTypeInteger', mkTypeInteger', mkTypeInteger'] (mkVar' 1),
          mkLambdas' [mkTypeInteger', mkTypeInteger'] (mkVar' 1)
        )
      ]
  ]
  where
    addLambdas :: Node -> Node
    addLambdas = rmap go
      where
        go :: ([BinderChange] -> Node -> Node) -> Node -> Node
        go recur node = case node of
          NLam {} -> mkLambda' mkDynamic' (recur [BCAdd 1] node)
          _ -> recur [] node

    replaceLambdasWithLets :: Node -> Node
    replaceLambdasWithLets = rmapN go
      where
        go :: ([BinderChange] -> Node -> Node) -> Level -> Node -> Node
        go recur k node = case node of
          NLam Lambda {..} ->
            mkLet
              _lambdaInfo
              (over binderType (cont []) _lambdaBinder)
              (mkConstant' (ConstInteger (fromIntegral k)))
              (cont [BCAdd 1, BCRemove (BinderRemove _lambdaBinder (mkVar' 0))] _lambdaBody)
          _ ->
            recur [] node
          where
            cont :: [BinderChange] -> Node -> Node
            cont bcs = go (recur . (bcs ++)) (k + bindersNumFromBinderChange bcs)

    removeLambdas :: Node -> Node
    removeLambdas = rmap go
      where
        go :: ([BinderChange] -> Node -> Node) -> Node -> Node
        go recur node = case node of
          NLam lam1@(Lambda _ _ (NLam lam2)) ->
            mkLambda
              (lam1 ^. lambdaInfo)
              (over binderType (cont []) (lam1 ^. lambdaBinder))
              ( cont
                  [ BCKeep (lam1 ^. lambdaBinder),
                    BCRemove (BinderRemove (lam2 ^. lambdaBinder) (mkVar' 0))
                  ]
                  (lam2 ^. lambdaBody)
              )
          _ ->
            recur [] node
          where
            cont :: [BinderChange] -> Node -> Node
            cont bcs = go (recur . (bcs ++))
