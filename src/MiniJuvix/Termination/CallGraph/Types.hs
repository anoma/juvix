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

data Call = Call {
  _callName :: A.Name,
  _callArgs :: [(Maybe Int, A.Expression)]
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
    ppArg :: (Maybe Int, A.Expression) -> Sem r (Doc Ann)
    ppArg (mi, a) = do
      showDecr <- asks _optShowDecreasingArgs
      case mi of
        Just i | showDecr -> do
          a' <- ppCode a
          return $ brackets (a' <+> kwPred <+> pretty i)
        _ -> ppCodeAtom a
    kwPred :: Doc Ann
    kwPred = annotate AnnKeyword "≺"

instance PrettyCode CallGraph where
  ppCode :: forall r. Members '[Reader Options] r => CallGraph -> Sem r (Doc Ann)
  ppCode (CallGraph m) = vsep <$> mapM ppEntry (HashMap.toList m)
    where
    ppEntry :: (A.FunctionName, [Call]) -> Sem r (Doc Ann)
    ppEntry (fun, calls) = do
      fun' <- annotate AnnImportant <$> ppSCode fun
      calls' <- vsep <$> mapM ppCode calls
      return $ fun' <+> pretty ("↝" :: Text) <+> align calls'
