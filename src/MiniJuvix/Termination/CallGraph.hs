{-# LANGUAGE TemplateHaskell #-}
module MiniJuvix.Termination.CallGraph where

import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Abstract.Language as A
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import qualified Data.HashMap.Strict as HashMap
import Prettyprinter

newtype CallGraph = CallGraph {
  _callGraph :: HashMap A.FunctionName [Call] }
  deriving newtype (Semigroup, Monoid)

data Call = Call {
  _callName :: A.FunctionName,
  _callArgs :: [A.Expression]
  }
makeLenses ''Call
makeLenses ''CallGraph

viewCall :: A.Expression -> Maybe Call
viewCall e = case e of
  -- A.ExpressionDefinedName f -> Just (Call f [])
  A.ExpressionApplication (A.Application f x) ->
    over callArgs (`snoc`x) <$> viewCall f
  _ -> Nothing

addCall :: A.FunctionName -> Call -> CallGraph -> CallGraph
addCall fun c = over callGraph (HashMap.insertWith (flip (<>)) fun [c])

registerCall :: Members '[State CallGraph, Reader A.FunctionName] r => Call -> Sem r ()
registerCall c = do
  fun <- ask
  modify (addCall fun c)

buildCallGraph :: A.TopModule -> CallGraph
buildCallGraph = run . execState mempty . checkModule

checkModule :: Members '[State CallGraph] r => A.TopModule -> Sem r ()
checkModule m = undefined

checkFunctionDef :: Members '[State CallGraph] r => A.FunctionDef -> Sem r ()
checkFunctionDef def =
  -- TODO should we check the type signature?
  runReader (def ^. A.funDefName) (mapM_ checkFunctionClause (def ^. A.funDefClauses))

checkFunctionClause :: Members '[State CallGraph, Reader A.FunctionName] r => A.FunctionClause -> Sem r ()
checkFunctionClause cl = checkExpression (cl ^. A.clauseBody)

checkExpression :: Members '[State CallGraph, Reader A.FunctionName] r => A.Expression -> Sem r ()
checkExpression e = do
  whenJust (viewCall e) registerCall
  case e of
    A.ExpressionApplication a -> checkApplication a
    A.ExpressionIden {} -> return ()
    A.ExpressionUniverse {} -> return ()
    A.ExpressionFunction f -> checkFunction f

checkApplication :: Members '[State CallGraph, Reader A.FunctionName] r => A.Application -> Sem r ()
checkApplication (A.Application l r) = do
  -- TODO recurse left?
  checkExpression l
  checkExpression r

checkFunction :: Members '[State CallGraph, Reader A.FunctionName] r => A.Function -> Sem r ()
checkFunction (A.Function l r) = do
  checkFunctionParameter l
  checkExpression r

checkFunctionParameter :: Members '[State CallGraph, Reader A.FunctionName] r => A.FunctionParameter -> Sem r ()
checkFunctionParameter p = checkExpression (p ^. A.paramType)

instance Pretty CallGraph where
  pretty (CallGraph m) = vsep (map ppEntry (HashMap.toList m))
    where
    ppEntry :: (A.FunctionName, [Call]) -> Doc a
    ppEntry (fun, calls) = "@" <> pretty (fun ^. S.nameId) <> colon <> align (vsep (map ppCall calls))
    ppCall :: Call -> Doc a
    ppCall c = "call @" <> pretty (c ^. callName . S.nameId) <+> pretty (length (c ^. callArgs))
