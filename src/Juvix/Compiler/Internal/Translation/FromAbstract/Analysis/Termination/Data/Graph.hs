module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Data.Graph
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Data.Graph,
  )
where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Abstract.Pretty.Base
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Data.FunctionCall
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Data.SizeRelation
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Prettyprinter as PP

type Graph = HashMap (FunctionName, FunctionName) Edge

data Edge = Edge
  { _edgeFrom :: FunctionName,
    _edgeTo :: FunctionName,
    _edgeMatrices :: HashSet CallMatrix
  }

newtype CompleteCallGraph = CompleteCallGraph Graph

data ReflexiveEdge = ReflexiveEdge
  { _reflexiveEdgeFun :: FunctionName,
    _reflexiveEdgeMatrices :: HashSet CallMatrix
  }

data RecursiveBehaviour = RecursiveBehaviour
  { _recursiveBehaviourFun :: FunctionName,
    _recursiveBehaviourMatrix :: [[Rel]]
  }
  deriving stock (Show)

makeLenses ''Edge
makeLenses ''RecursiveBehaviour
makeLenses ''ReflexiveEdge

instance PrettyCode Edge where
  ppCode Edge {..} = do
    fromFun <- ppCode _edgeFrom
    toFun <- ppCode _edgeTo
    matrices <-
      indent 2 . ppMatrices . zip [0 :: Int ..]
        <$> mapM ppCode (toList _edgeMatrices)
    return $
      pretty ("Edge" :: Text)
        <+> fromFun
        <+> kwWaveArrow
        <+> toFun
          <> line
          <> matrices
    where
      ppMatrices :: [(Int, Doc a)] -> Doc a
      ppMatrices = vsep2 . map ppMatrix
      ppMatrix :: (Int, Doc ann) -> Doc ann
      ppMatrix (i, t) =
        pretty ("Matrix" :: Text) <+> pretty i <> colon <> line <> t

instance PrettyCode CompleteCallGraph where
  ppCode :: forall r. Members '[Reader Options] r => CompleteCallGraph -> Sem r (Doc Ann)
  ppCode (CompleteCallGraph edges)
    | null edges = return $ pretty ("Empty graph" :: Text)
    | otherwise = do
        es <- vsep2 <$> mapM ppCode (toList edges)
        return $ pretty ("Complete call graph:" :: Text) <> line <> es <> pretty (length edges)

instance PrettyCode RecursiveBehaviour where
  ppCode :: Members '[Reader Options] r => RecursiveBehaviour -> Sem r (Doc Ann)
  ppCode (RecursiveBehaviour f m0) = do
    f' <- ppCode f
    let m' = PP.vsep (map (PP.list . map pretty) m)
    return $
      pretty ("Recursive behaviour of" :: Text)
        <+> f'
          <> colon
          <> line
          <> indent 2 (align m')
    where
      m = toList (HashSet.fromList m0)
