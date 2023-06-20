module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.FunctionCall (module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.FunctionCall) where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Pretty.Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.SizeRelation
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

type FunctionRef = FunctionName

newtype CallMap = CallMap
  { _callMap :: HashMap FunctionRef (HashMap FunctionRef [FunCall])
  }
  deriving newtype (Semigroup, Monoid)

data FunCall = FunCall
  { _callRef :: FunctionRef,
    _callArgs :: [(CallRow, Expression)]
  }

newtype CallRow = CallRow
  { _callRow :: Maybe (Int, SizeRel')
  }
  deriving stock (Eq, Show, Generic)

instance Hashable CallRow

newtype CallMatrix = CallMatrix
  { _unCallMatrix :: [CallRow]
  }
  deriving newtype (Eq, Show, Hashable)

data Call = Call
  { _callFrom :: FunctionName,
    _callTo :: FunctionName,
    _callMatrix :: CallMatrix
  }

newtype LexOrder = LexOrder (NonEmpty Int)

makeLenses ''CallMatrix
makeLenses ''CallRow
makeLenses ''FunCall
makeLenses ''CallMap

filterCallMap :: (Foldable f) => f Text -> CallMap -> CallMap
filterCallMap funNames =
  over
    callMap
    ( HashMap.filterWithKey
        ( \k _ ->
            (k ^. nameText)
              `elem` funNames
        )
    )

instance PrettyCode FunCall where
  ppCode ::
    forall r.
    (Members '[Reader Options] r) =>
    FunCall ->
    Sem r (Doc Ann)
  ppCode (FunCall f args) = do
    args' <- mapM ppArg args
    f' <- ppCode f
    return $ f' <+> hsep args'
    where
      ppArg :: (CallRow, Expression) -> Sem r (Doc Ann)
      ppArg (CallRow mi, a) =
        case mi of
          Just (i, r) -> relAux (Just i) (RJust r)
          Nothing -> relAux Nothing RNothing
        where
          relAux mayIx r = do
            showDecr <- asks (^. optShowDecreasingArgs)
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
  ppCode ::
    forall r.
    (Members '[Reader Options] r) =>
    CallMap ->
    Sem r (Doc Ann)
  ppCode (CallMap m) = vsep <$> mapM ppEntry (HashMap.toList m)
    where
      ppEntry :: (FunctionRef, HashMap FunctionRef [FunCall]) -> Sem r (Doc Ann)
      ppEntry (fun, mcalls) = do
        fun' <- annotate AnnImportant <$> ppCode fun
        calls' <- vsep <$> mapM ppCode calls
        return $ fun' <+> kwWaveArrow <+> align calls'
        where
          calls = concat (HashMap.elems mcalls)

instance PrettyCode CallRow where
  ppCode (CallRow r) = return $ case r of
    Nothing -> kwQuestion
    Just (i, r') ->
      pretty i <+> annotate AnnKeyword (pretty r')

instance PrettyCode CallMatrix where
  ppCode l = vsep <$> mapM ppCode (l ^. unCallMatrix)

kwQuestion :: Doc Ann
kwQuestion = keyword Str.questionMark

kwWaveArrow :: Doc Ann
kwWaveArrow = keyword Str.waveArrow
