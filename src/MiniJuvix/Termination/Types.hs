{-# LANGUAGE TemplateHaskell #-}
module MiniJuvix.Termination.Types (
  module MiniJuvix.Termination.Types.SizeRelation,
  module MiniJuvix.Termination.Types
                                             ) where

import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Abstract.Language as A
import qualified Data.HashMap.Strict as HashMap
import Prettyprinter
import MiniJuvix.Termination.Types.SizeRelation
import MiniJuvix.Syntax.Abstract.Pretty.Base

newtype CallMap = CallMap {
  _callMap :: HashMap A.FunctionName (HashMap A.FunctionName [FunCall]) }
  deriving newtype (Semigroup, Monoid)

data FunCall = FunCall {
  _callName :: A.FunctionName,
  _callArgs :: [(CallRow, A.Expression)]
  }

newtype CallRow = CallRow {
  _callRow :: Maybe (Int, Rel')
  }

type CallMatrix = [CallRow]

data Call = Call {
  _callFrom :: A.FunctionName,
  _callTo :: A.FunctionName,
  _callMatrix :: CallMatrix
  }
type Edges = HashMap (A.FunctionName, A.FunctionName) Edge

data Edge = Edge {
  _edgeFrom :: A.FunctionName,
  _edgeTo :: A.FunctionName,
  _edgeMatrices :: [CallMatrix]
  }

newtype CompleteCallGraph = CompleteCallGraph Edges

data ReflexiveEdge = ReflexiveEdge {
  _redgeMatrix :: CallMatrix,
  _redgeFun :: A.FunctionName
  }

makeLenses ''FunCall
makeLenses ''Edge
makeLenses ''CallMap

instance PrettyCode FunCall where
  ppCode :: forall r. Members '[Reader Options] r => FunCall -> Sem r (Doc Ann)
  ppCode (FunCall f args) = do
    args' <- mapM ppArg args
    f' <- ppSCode f
    return $ f' <+> hsep args'
    where
    ppArg :: (CallRow, A.Expression) -> Sem r (Doc Ann)
    ppArg (CallRow mi, a) =
      case mi of
        Just (i, r) -> relAux (Just i) (RJust r)
        Nothing -> relAux Nothing RNothing
      where
      relAux mayIx r = do
        showDecr <- asks _optShowDecreasingArgs
        let pi = annotate AnnImportant . pretty <$> mayIx
            pr = annotate AnnKeyword (pretty r)
        case showDecr of
          OnlyArg -> ppCodeAtom a
          OnlyRel -> return $ dbrackets (pr <+?> pi)
          ArgRel -> do
            a' <- ppCode a
            return $ dbrackets (a' <+> pr <+?> pi)
    dbrackets :: Doc a -> Doc a
    dbrackets x = pretty '⟦' <> x <> pretty '⟧'

instance PrettyCode CallMap where
  ppCode :: forall r. Members '[Reader Options] r => CallMap -> Sem r (Doc Ann)
  ppCode (CallMap m) = vsep <$> mapM ppEntry (HashMap.toList m)
    where
    ppEntry :: (A.FunctionName, HashMap A.FunctionName [FunCall]) -> Sem r (Doc Ann)
    ppEntry (fun, mcalls) = do
      fun' <- annotate AnnImportant <$> ppSCode fun
      calls' <- vsep <$> mapM ppCode calls
      return $ fun' <+> waveFun <+> align calls'
      where
      calls = concat (HashMap.elems mcalls)

kwQuestion :: Doc Ann
kwQuestion = annotate AnnKeyword "?"

waveFun :: Doc ann
waveFun = pretty ("↝" :: Text)

vsep2 :: [Doc ann] -> Doc ann
vsep2 = concatWith (\a b -> a <> line <> line <> b)

instance PrettyCode CallRow where
  ppCode (CallRow r) = return $ case r of
    Nothing -> kwQuestion
    Just (i, r') ->
      pretty i <+> annotate AnnKeyword (pretty r')

instance PrettyCode CallMatrix where
  ppCode l = vsep <$> mapM ppCode l

instance PrettyCode Edge where
  ppCode Edge {..} = do
    fromFun <- ppSCode _edgeFrom
    toFun <- ppSCode _edgeTo
    matrices <- indent 2 . ppMatrices . zip [0 :: Int ..] <$> mapM ppCode _edgeMatrices
    return $ pretty ("Edge" :: Text) <+> fromFun <+> waveFun <+> toFun <> line
      <> matrices
    where
    ppMatrices = vsep2 . map ppMatrix
    ppMatrix (i, t) = pretty ("Matrix" :: Text) <+> pretty i <> colon <> line
      <> t

instance PrettyCode CompleteCallGraph where
  ppCode :: forall r. Members '[Reader Options] r => CompleteCallGraph -> Sem r (Doc Ann)
  ppCode (CompleteCallGraph edges) = do
    es <- vsep2 <$> mapM ppCode (toList edges)
    return $ pretty ("Complete Call Graph:" :: Text) <> line <> es
