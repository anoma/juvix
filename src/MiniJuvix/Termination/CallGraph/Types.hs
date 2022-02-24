{-# LANGUAGE TemplateHaskell #-}
module MiniJuvix.Termination.CallGraph.Types where

import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Abstract.Language as A
import qualified Data.HashMap.Strict as HashMap
import Prettyprinter
import MiniJuvix.Syntax.Abstract.Pretty.Base

newtype CallGraph = CallGraph {
  _callGraph :: HashMap A.FunctionName [Call] }
  deriving newtype (Semigroup, Monoid)

data Argument = Argument {
  _argOwnerFunction :: A.FunctionName,
  _argIx :: Int
  }

data Rel =
  REq
  | RLe
  | RNothing

data ArgRelation =
  LessThan Int
  | EqualTo Int
  | DontKnow

data Call = Call {
  _callName :: A.Name,
  _callArgs :: [(ArgRelation, A.Expression)]
  }
makeLenses ''Call
makeLenses ''CallGraph

instance PrettyCode Call where
  ppCode :: forall r. Members '[Reader Options] r => Call -> Sem r (Doc Ann)
  ppCode (Call f args) = do
    args' <- mapM ppArg args
    f' <- ppSCode f
    return $ f' <+> hsep args'
    where
    ppArg :: (ArgRelation, A.Expression) -> Sem r (Doc Ann)
    ppArg (mi, a) =
      case mi of
        LessThan i -> relAux kwPred (Just i)
        EqualTo i -> relAux kwEqual (Just i)
        DontKnow -> relAux kwQuestion Nothing
      where
      relAux kwRel mayIx = do
        showDecr <- asks _optShowDecreasingArgs
        let pi = annotate AnnImportant . pretty <$> mayIx
        case showDecr of
          OnlyArg -> ppCodeAtom a
          OnlyRel -> return $ dbrackets (kwRel <+?> pi)
          ArgRel -> do
            a' <- ppCode a
            return $ dbrackets (a' <+> kwRel <+?> pi)
    dbrackets :: Doc a -> Doc a
    dbrackets x = pretty '⟦' <> x <> pretty '⟧'
    kwPred :: Doc Ann
    kwPred = annotate AnnKeyword "≺"
    kwEqual :: Doc Ann
    kwEqual = annotate AnnKeyword "∼"
    kwQuestion :: Doc Ann
    kwQuestion = annotate AnnKeyword "?"

instance PrettyCode CallGraph where
  ppCode :: forall r. Members '[Reader Options] r => CallGraph -> Sem r (Doc Ann)
  ppCode (CallGraph m) = vsep <$> mapM ppEntry (HashMap.toList m)
    where
    ppEntry :: (A.FunctionName, [Call]) -> Sem r (Doc Ann)
    ppEntry (fun, calls) = do
      fun' <- annotate AnnImportant <$> ppSCode fun
      calls' <- vsep <$> mapM ppCode calls
      return $ fun' <+> pretty ("↝" :: Text) <+> align calls'
