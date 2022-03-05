{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module MiniJuvix.Termination.Types (
  module MiniJuvix.Termination.Types.SizeRelation,
  module MiniJuvix.Termination.Types
                                             ) where

import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Abstract.Language as A
import qualified Data.HashMap.Strict as HashMap
import Prettyprinter as PP
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
  deriving stock (Eq, Show, Generic)

instance Hashable CallRow

type CallMatrix = [CallRow]

data Call = Call {
  _callFrom :: A.FunctionName,
  _callTo :: A.FunctionName,
  _callMatrix :: CallMatrix
  }
newtype LexOrder = LexOrder (NonEmpty Int)

makeLenses ''FunCall
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
