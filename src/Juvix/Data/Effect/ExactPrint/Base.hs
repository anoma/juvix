module Juvix.Data.Effect.ExactPrint.Base
  ( module Juvix.Data.Effect.ExactPrint.Base,
    module Juvix.Data.Loc,
    module Juvix.Data.Comment,
  )
where

import Juvix.Data.CodeAnn hiding (line')
import Juvix.Data.Comment
import Juvix.Data.Loc
import Juvix.Prelude.Base
import Prettyprinter qualified as P

data ExactPrint m a where
  NoLoc :: Doc Ann -> ExactPrint m ()
  Morpheme :: Interval -> Doc Ann -> ExactPrint m ()
  PrintCommentsUntil :: Interval -> ExactPrint m (Maybe SpaceSpan)
  Region :: (Doc Ann -> Doc Ann) -> m b -> ExactPrint m b
  End :: ExactPrint m ()

makeSem ''ExactPrint

data Builder = Builder
  { -- | comments sorted by starting location
    _builderComments :: [SpaceSpan],
    _builderDoc :: Doc Ann,
    _builderEnd :: FileLoc
  }

makeLenses ''Builder

runExactPrint :: Maybe FileComments -> Sem (ExactPrint ': r) x -> Sem r (Doc Ann, x)
runExactPrint cs = fmap (first (^. builderDoc)) . runState ini . re
  where
    ini :: Builder
    ini =
      Builder
        { _builderComments = fromMaybe [] (cs ^? _Just . fileCommentsSorted),
          _builderDoc = mempty,
          _builderEnd = FileLoc 0 0 0
        }

execExactPrint :: Maybe FileComments -> Sem (ExactPrint ': r) x -> Sem r (Doc Ann)
execExactPrint cs = fmap fst . runExactPrint cs

re :: forall r a. Sem (ExactPrint ': r) a -> Sem (State Builder ': r) a
re = reinterpretH h
  where
    h ::
      forall rInitial x.
      ExactPrint (Sem rInitial) x ->
      Tactical ExactPrint (Sem rInitial) (State Builder ': r) x
    h = \case
      NoLoc p -> append' p >>= pureT
      Morpheme l p -> morpheme' l p >>= pureT
      End -> end' >>= pureT
      PrintCommentsUntil l -> printCommentsUntil' l >>= pureT
      Region f m -> do
        st0 :: Builder <- set builderDoc mempty <$> get
        m' <- runT m
        (st' :: Builder, fx) <- raise (evalExactPrint' st0 m')

        modify (over builderDoc (<> f (st' ^. builderDoc)))
        modify (set builderComments (st' ^. builderComments))
        modify (set builderEnd (st' ^. builderEnd))

        return fx

evalExactPrint' :: Builder -> Sem (ExactPrint ': r) a -> Sem r (Builder, a)
evalExactPrint' b = runState b . re

append' :: forall r. Members '[State Builder] r => Doc Ann -> Sem r ()
append' d = modify (over builderDoc (<> d))

line' :: forall r. Members '[State Builder] r => Sem r ()
line' = append' P.line

-- | It prints all remaining comments
end' :: forall r. Members '[State Builder] r => Sem r ()
end' = do
  cs <- gets (^. builderComments)
  case cs of
    [] -> return ()
    [x] -> printSpaceSpan x
    _ -> impossible
  modify' (set builderComments [])

printSpaceSpan :: forall r. Members '[State Builder] r => SpaceSpan -> Sem r ()
printSpaceSpan = sequenceWith (return ()) . fmap printSpaceSection . (^. spaceSpan)
  where
    printSpaceSection :: SpaceSection -> Sem r ()
    printSpaceSection = \case
      SpaceComment c -> printComment c
      SpaceLines l -> append' (pretty (l ^. emptyLinesNum)) >> line'

printComment :: Members '[State Builder] r => Comment -> Sem r ()
printComment c = do
  append' (annotate AnnComment (P.pretty c))
  line'

printCommentsUntil' :: forall r. Members '[State Builder] r => Interval -> Sem r (Maybe SpaceSpan)
printCommentsUntil' loc = go
  where
    go :: Sem r (Maybe SpaceSpan)
    go = do
      g <- popSpaceSpan
      whenJust g printSpaceSpan
      return g
      where
        cmp :: SpaceSpan -> Bool
        cmp c = getLoc c ^. intervalStart < loc ^. intervalStart

        popSpaceSpan :: Sem r (Maybe SpaceSpan)
        popSpaceSpan = do
          cs <- gets (^. builderComments)
          case cs of
            h : hs
              | cmp h -> do
                  modify' (set builderComments hs)
                  return (Just h)
            _ -> return Nothing

morpheme' :: forall r. Members '[State Builder] r => Interval -> Doc Ann -> Sem r ()
morpheme' loc doc = do
  void (printCommentsUntil' loc)
  append' doc
