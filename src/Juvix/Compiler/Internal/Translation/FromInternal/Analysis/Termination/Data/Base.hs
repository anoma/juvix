{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.Base where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Pretty.Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.SizeRelation
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

newtype CallMap' expr = CallMap
  { _callMap :: HashMap FunctionName (HashMap FunctionName [FunCall' expr])
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

data CallMapBuilder' expr :: Effect where
  AddCall :: FunctionName -> FunCall' expr -> CallMapBuilder' expr m ()

makeEffect ''CallMapBuilder'

makeLenses ''CallMatrix
makeLenses ''CallRow
makeLenses ''FunCall'
makeLenses ''CallMap'
makeLenses ''FunCallArg'

mkFunCall :: forall pattrn expr. (pattrn -> expr -> Maybe SizeRel') -> FunctionName -> [pattrn] -> [expr] -> FunCall' expr
mkFunCall rel fun pats args =
  FunCall
    { _callRef = fun,
      _callArgs = map (mkFunCallArg rel pats) args
    }

mkFunCallArg :: forall pattrn expr. (pattrn -> expr -> Maybe SizeRel') -> [pattrn] -> expr -> FunCallArg' expr
mkFunCallArg rel pats arg =
  let rels = map (`rel` arg) pats
      helper srel = (,srel) <$> elemIndex (Just srel) rels
      smaller = helper RLe
      equal = helper REq
   in FunCallArg
        { _argExpression = arg,
          _argRow =
            CallRow $
              smaller
                <|> equal
        }

execCallMapBuilder :: Sem (CallMapBuilder' expr ': r) a -> Sem r (CallMap' expr)
execCallMapBuilder = fmap fst . runCallMapBuilder

runCallMapBuilder :: Sem (CallMapBuilder' expr ': r) a -> Sem r (CallMap' expr, a)
runCallMapBuilder = reinterpret (runState emptyCallMap) $ \case
  AddCall fun c -> modify (addCall' fun c)

addCall' :: forall expr. FunctionName -> FunCall' expr -> CallMap' expr -> CallMap' expr
addCall' fun c = over callMap (HashMap.alter (Just . insertCall c) fun)
  where
    insertCall ::
      FunCall' expr ->
      Maybe (HashMap FunctionName [FunCall' expr]) ->
      HashMap FunctionName [FunCall' expr]
    insertCall f = \case
      Nothing -> singl f
      Just m' -> addFunCall f m'

    singl :: FunCall' expr -> HashMap FunctionName [FunCall' expr]
    singl f = HashMap.singleton (f ^. callRef) [f]

    addFunCall ::
      FunCall' expr ->
      HashMap FunctionName [FunCall' expr] ->
      HashMap FunctionName [FunCall' expr]
    addFunCall fc = HashMap.insertWith (flip (<>)) (fc ^. callRef) [fc]

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
  ppCode (CallMap m) = vsep <$> mapM ppEntry (HashMap.toList m)
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
    { _callMap = mempty
    }
