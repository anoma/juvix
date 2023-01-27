module Juvix.Compiler.Core.Transformation.MatchToCase where

import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra (mkLet, shift)
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Recursors.Map.Named
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base
import Safe (headMay)
import Juvix.Compiler.Core.Info.NameInfo (setInfoName)

type MatchValues = [Node]

data MatchPattern
  = MatchCaseBinder CaseBinder
  | MatchCaseExpr CaseExpr

data CaseBinder = CaseBinder
  { _caseBinderBinder :: Binder,
    _caseBinderNode :: Node
  }

data CaseExpr = CaseExpr
  { _caseExprTag :: Tag,
    _caseExprValue :: Node,
    _caseExprBinders :: [Binder],
    _caseExprCtorName :: Text,
    _caseExprSymbol :: Symbol
  }

makeLenses ''CaseBinder
makeLenses ''CaseExpr

matchToCase :: InfoTable -> InfoTable
matchToCase = run . mapT' (const (dmapM matchToCaseNode))

matchToCaseNode :: forall r. Member InfoTableBuilder r => Node -> Sem r Node
matchToCaseNode n = case n of
  NMatch m -> do
    let branches = m ^. matchBranches
    let values = toList (m ^. matchValues)

    let numValues = length values
    caseNodes <- mapM (matchBranchToCaseNode values) (reverse branches)

    let bindings = failNode numValues : caseNodes

    let appNode = mkApps' (mkVar' 0) (shift (length bindings) <$> (reverse values))
    return (foldr mkLet' appNode bindings)
  _ -> return n

withBinderCount :: [MatchPattern] -> [(Int, MatchPattern)]
withBinderCount ps = zip binderCounts ps
  where
    binderCount :: MatchPattern -> Int
    binderCount = \case
      MatchCaseBinder {} -> 1
      MatchCaseExpr e -> length (e ^. caseExprBinders)

    binderCounts :: [Int]
    binderCounts = 0 : scanl1 (+) (binderCount <$> ps)


matchBranchToCaseNode :: forall r. Members '[InfoTableBuilder] r => MatchValues -> MatchBranch -> Sem r Node
matchBranchToCaseNode vs b= do
  let patterns = toList (b ^. matchBranchPatterns)
  let body :: Node = b ^. matchBranchBody
  specs <- (concatMapM (uncurry extractBinders) (zipExact vs patterns))
  let cbs = withBinderCount specs
  return $ mkLambdas' (length vs) (foldr' (compileMatchPattern (length vs)) body cbs)

failNode :: Int -> Node
failNode n = mkLambdas' n (mkBuiltinApp' OpFail [mkConstant' (ConstString "Non-exhaustive patterns")])

defaultNode :: Int -> Int -> Node
defaultNode numMatchValues numBindersAbove = mkApps' (mkVar' (numBindersAbove + numMatchValues)) (mkVar' <$> (reverse (take numMatchValues [numBindersAbove..])))

combineMatchPatterns :: Node -> (Int, MatchPattern) -> Node -> Node
combineMatchPatterns d (i, mp) n = case mp of
  MatchCaseBinder cb -> mkLet mempty (cb ^. caseBinderBinder) (shift i (cb ^. caseBinderNode)) n
  MatchCaseExpr e -> mkCase mempty (e ^. caseExprSymbol) (shift i (e ^. caseExprValue)) [b] (Just d)
    where
      b :: CaseBranch
      b =
        CaseBranch
          { _caseBranchInfo = setInfoName (e ^. caseExprCtorName) mempty,
            _caseBranchTag = e ^. caseExprTag,
            _caseBranchBinders = e ^. caseExprBinders,
            _caseBranchBindersNum = length (e ^. caseExprBinders),
            _caseBranchBody = n
          }

compileMatchPattern :: Int -> (Int, MatchPattern) -> Node -> Node
compileMatchPattern numValues (numBindersAbove, p) nextNode = case p of
  MatchCaseBinder cb -> mkLet mempty (cb ^. caseBinderBinder) (shift numBindersAbove (cb ^. caseBinderNode)) nextNode
  MatchCaseExpr e -> mkCase mempty (e ^. caseExprSymbol) (shift numBindersAbove (e ^. caseExprValue)) [b] (Just (defaultNode numValues numBindersAbove))
    where
      b :: CaseBranch
      b =
        CaseBranch
          { _caseBranchInfo = setInfoName (e ^. caseExprCtorName) mempty,
            _caseBranchTag = e ^. caseExprTag,
            _caseBranchBinders = e ^. caseExprBinders,
            _caseBranchBindersNum = length (e ^. caseExprBinders),
            _caseBranchBody = nextNode
          }

extractBinders :: forall r. Member InfoTableBuilder r => Node -> Pattern -> Sem r [MatchPattern]
extractBinders v p = case p of
  PatBinder b ->
    return
      [ MatchCaseBinder $
          CaseBinder
            { _caseBinderBinder = b ^. patternBinder,
              _caseBinderNode = v
            }
      ]
  PatWildcard {} -> return []
  PatConstr c -> do
    ctorInfo <- getConstructorInfo (c ^. patternConstrTag)
    let binders = [b ^. patternBinder | (PatBinder b) <- c ^. patternConstrArgs]
    return
      [ MatchCaseExpr $
          CaseExpr
            { _caseExprTag = c ^. patternConstrTag,
              _caseExprValue = v,
              _caseExprBinders = binders,
              _caseExprSymbol = ctorInfo ^. constructorInductive,
              _caseExprCtorName = ctorInfo ^. constructorName
            }
      ]
