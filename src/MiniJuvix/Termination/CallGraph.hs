module MiniJuvix.Termination.CallGraph (
module MiniJuvix.Termination.CallGraph.Types ,
module MiniJuvix.Termination.CallGraph
                                       ) where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language.Extra
import qualified Data.HashMap.Strict as HashMap
import MiniJuvix.Termination.CallGraph.Types

viewCall :: Expression -> Maybe Call
viewCall e = case e of
  ExpressionApplication (Application f x) ->
    over callArgs (`snoc`x) <$> viewCall f
  ExpressionIden (IdenDefined x) ->
     Just (singletonCall x)
  _ -> Nothing
  where
  singletonCall :: Name -> Call
  singletonCall n = Call n []

addCall :: FunctionName -> Call -> CallGraph -> CallGraph
addCall fun c = over callGraph (HashMap.insertWith (flip (<>)) fun [c])

registerCall :: Members '[State CallGraph, Reader FunctionName] r => Call -> Sem r ()
registerCall c = do
  fun <- ask
  modify (addCall fun c)

buildCallGraph :: TopModule -> CallGraph
buildCallGraph = run . execState mempty . checkModule

checkModule :: Members '[State CallGraph] r => TopModule -> Sem r ()
checkModule m = checkModuleBody (m ^. moduleBody)

checkModuleBody :: Members '[State CallGraph] r => ModuleBody -> Sem r ()
checkModuleBody body = do
  mapM_ checkFunctionDef (toList $ body ^. moduleFunctions)
  mapM_ checkLocalModule (toList $ body ^. moduleLocalModules)

checkLocalModule :: Members '[State CallGraph] r => LocalModule -> Sem r ()
checkLocalModule m = checkModuleBody (m ^. moduleBody)

checkFunctionDef :: Members '[State CallGraph] r => FunctionDef -> Sem r ()
checkFunctionDef def = runReader (def ^. funDefName) $ do
  checkTypeSignature (def ^. funDefTypeSig)
  mapM_ checkFunctionClause (def ^. funDefClauses)

checkTypeSignature :: Members '[State CallGraph, Reader FunctionName] r => Expression -> Sem r ()
checkTypeSignature = checkExpression

checkFunctionClause :: Members '[State CallGraph, Reader FunctionName] r => FunctionClause -> Sem r ()
checkFunctionClause cl = checkExpression (cl ^. clauseBody)

checkExpression :: Members '[State CallGraph, Reader FunctionName] r => Expression -> Sem r ()
checkExpression e =
  case viewCall e of
    Just c -> do registerCall c
                 mapM_ checkExpression (c ^. callArgs)
    Nothing -> case e of
      ExpressionApplication a -> checkApplication a
      ExpressionIden {} -> return ()
      ExpressionUniverse {} -> return ()
      ExpressionFunction f -> checkFunction f

checkApplication :: Members '[State CallGraph, Reader FunctionName] r => Application -> Sem r ()
checkApplication (Application l r) = do
  checkExpression l
  checkExpression r

checkFunction :: Members '[State CallGraph, Reader FunctionName] r => Function -> Sem r ()
checkFunction (Function l r) = do
  checkFunctionParameter l
  checkExpression r

checkFunctionParameter :: Members '[State CallGraph, Reader FunctionName] r => FunctionParameter -> Sem r ()
checkFunctionParameter p = checkExpression (p ^. paramType)
