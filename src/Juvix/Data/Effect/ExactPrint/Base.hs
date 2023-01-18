module Juvix.Data.Effect.ExactPrint.Base
  ( module Juvix.Data.Effect.ExactPrint.Base,
    module Juvix.Data.Loc,
    module Juvix.Data.Comment,
  )
where

import Juvix.Data.Comment
import Juvix.Data.Loc
import Juvix.Prelude.Base
import Prettyprinter qualified as P

data ExactPrint ann m a where
  NoLoc :: Doc ann -> ExactPrint ann m ()
  Morpheme :: Interval -> Doc ann -> ExactPrint ann m ()
  Region :: (Doc ann -> Doc ann) -> m b -> ExactPrint ann m b

makeSem ''ExactPrint

data Builder ann = Builder
  { -- | comments sorted by starting location
    _builderComments :: [Comment],
    _builderDoc :: Doc ann,
    _builderEnd :: FileLoc
  }

makeLenses ''Builder

runExactPrint :: FileComments -> Sem (ExactPrint ann ': r) x -> Sem r (Doc ann, x)
runExactPrint cs = fmap (first (^. builderDoc)) . runState ini . re
  where
    ini :: Builder ann
    ini =
      Builder
        { _builderComments = cs ^. fileCommentsSorted,
          _builderDoc = mempty,
          _builderEnd = FileLoc 0 0 0
        }

execExactPrint :: FileComments -> Sem (ExactPrint ann ': r) x -> Sem r (Doc ann)
execExactPrint cs = fmap fst . runExactPrint cs

re :: forall ann r a. Sem (ExactPrint ann ': r) a -> Sem (State (Builder ann) ': r) a
re = reinterpretH h
  where
    h ::
      forall rInitial x.
      ExactPrint ann (Sem rInitial) x ->
      Tactical (ExactPrint ann) (Sem rInitial) (State (Builder ann) ': r) x
    h = \case
      NoLoc p -> append' p >>= pureT
      Morpheme l p -> morpheme' l p >>= pureT
      Region f m -> do
        st0 :: Builder ann <- set builderDoc mempty <$> get @(Builder ann)
        m' <- runT m
        (st' :: Builder ann, fx) <- raise $ evalExactPrint' st0 m'

        modify @(Builder ann) (over builderDoc (<> f (st' ^. builderDoc)))
        modify @(Builder ann) (set builderComments (st' ^. builderComments))
        modify @(Builder ann) (set builderEnd (st' ^. builderEnd))

        return fx

evalExactPrint' :: Builder ann -> Sem (ExactPrint ann ': r) a -> Sem r (Builder ann, a)
evalExactPrint' b = runState b . re

-- TODO add new lines?
eprint :: forall ann r. Members '[State (Builder ann)] r => Interval -> Doc ann -> Sem r ()
eprint _loc doc = append' doc

-- where
-- number of lines between two intervals. 0 if they are on the same line
-- it is assumed that a comes before b (i.e. a < b)
-- numLines :: Interval -> Interval -> Int
-- numLines a b = intervalStartLine b - intervalEndLine a

append' :: forall ann r. Members '[State (Builder ann)] r => Doc ann -> Sem r ()
append' d = modify (over builderDoc (<> d))

line' :: forall ann r. Members '[State (Builder ann)] r => Proxy ann -> Sem r ()
line' _ = append' @ann P.line

morpheme' :: forall ann r. Members '[State (Builder ann)] r => Interval -> Doc ann -> Sem r ()
morpheme' loc doc = do
  mc <- popComment
  case mc of
    Nothing -> eprint loc doc
    Just c -> printComment c >> morpheme' loc doc
  where
    cmp :: Comment -> Bool
    cmp c = c ^. commentInterval . intervalStart < loc ^. intervalStart

    printComment :: Comment -> Sem r ()
    printComment c = eprint @ann (c ^. commentInterval) (P.pretty (c ^. commentText))

    popComment :: Sem r (Maybe Comment)
    popComment = do
      cs <- gets @(Builder ann) (^. builderComments)
      case cs of
        (h : hs)
          | cmp h -> do
              modify' @(Builder ann) (set builderComments hs)
              return (Just h)
        _ -> return Nothing
