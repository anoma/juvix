module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.Base where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Pretty.Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.SizeRelation
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

data CallMap' expr = CallMap
  { _callMap :: HashMap FunctionName (HashMap FunctionName [FunCall' expr]),
    _callMapScanned :: HashMap FunctionName FunctionDef
  }

data FunCall' expr = FunCall
  { _callRef :: FunctionName,
    _callArgs :: [FunCallArg' expr]
  }

data FunCallArg' expr = FunCallArg
  { _argRow :: CallRow,
    _argExpression :: expr
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
makeLenses ''FunCall'
makeLenses ''CallMap'
makeLenses ''FunCallArg'

filterCallMap :: (Foldable f) => f Text -> CallMap' expr -> CallMap' expr
filterCallMap funNames =
  over
    callMap
    ( HashMap.filterWithKey
        ( \k _ ->
            (k ^. nameText)
              `elem` funNames
        )
    )

instance (HasAtomicity expr, PrettyCode expr) => PrettyCode (FunCall' expr) where
  ppCode ::
    forall r.
    (Members '[Reader Options] r) =>
    FunCall' expr ->
    Sem r (Doc Ann)
  ppCode (FunCall f args) = do
    args' <- mapM ppArg args
    f' <- ppCode f
    return $ f' <+> hsep args'
    where
      ppArg :: FunCallArg' expr -> Sem r (Doc Ann)
      ppArg arg =
        case mi of
          Just (i, r) -> relAux (Just i) (RJust r)
          Nothing -> relAux Nothing RNothing
        where
          argExpr = arg ^. argExpression
          mi = arg ^. argRow . callRow
          relAux mayIx r = do
            showDecr <- asks (^. optShowDecreasingArgs)
            let pi = annotate AnnImportant . pretty <$> mayIx
                pr = annotate AnnKeyword (pretty r)
            case showDecr of
              OnlyArg -> ppCodeAtom argExpr
              OnlyRel -> return $ dbrackets (pr <+?> pi)
              ArgRel -> do
                a' <- ppCode argExpr
                return $ dbrackets (a' <+> pr <+?> pi)
      dbrackets :: Doc a -> Doc a
      dbrackets x = pretty '⟦' <> x <> pretty '⟧'

instance (HasAtomicity expr, PrettyCode expr) => PrettyCode (CallMap' expr) where
  ppCode ::
    forall r.
    (Members '[Reader Options] r) =>
    CallMap' expr ->
    Sem r (Doc Ann)
  ppCode (CallMap m _) = vsep <$> mapM ppEntry (HashMap.toList m)
    where
      ppEntry :: (FunctionName, HashMap FunctionName [FunCall' expr]) -> Sem r (Doc Ann)
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

emptyCallMap :: CallMap' expr
emptyCallMap =
  CallMap
    { _callMap = mempty,
      _callMapScanned = mempty
    }
