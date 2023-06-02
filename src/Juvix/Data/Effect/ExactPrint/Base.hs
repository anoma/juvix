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
  EnsureEmptyLine :: ExactPrint m ()
  Region :: (Doc Ann -> Doc Ann) -> m b -> ExactPrint m b
  End :: ExactPrint m ()

makeSem ''ExactPrint

data Builder = Builder
  { -- | comments sorted by starting location
    _builderComments :: [SpaceSpan],
    _builderDoc :: Doc Ann,
    _builderEnsureEmptyLine :: Bool,
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
          _builderEnsureEmptyLine = False,
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
      EnsureEmptyLine -> modify' (set builderEnsureEmptyLine True) >>= pureT
      Morpheme l p -> morpheme' l p >>= pureT
      End -> end' >>= pureT
      PrintCommentsUntil l -> printCommentsUntil' l >>= pureT
      Region f m -> do
        st0 :: Builder <- set builderDoc mempty <$> get
        m' <- runT m
        (st' :: Builder, fx) <- raise (evalExactPrint' st0 m')
        doc' <- gets (^. builderDoc)
        put
          Builder
            { _builderDoc = doc' <> f (st' ^. builderDoc),
              _builderComments = st' ^. builderComments,
              _builderEnd = st' ^. builderEnd,
              _builderEnsureEmptyLine = st' ^. builderEnsureEmptyLine
            }
        return fx

evalExactPrint' :: Builder -> Sem (ExactPrint ': r) a -> Sem r (Builder, a)
evalExactPrint' b = runState b . re

append' :: forall r. Members '[State Builder] r => Doc Ann -> Sem r ()
append' d = modify (over builderDoc (<> d))

hardline' :: forall r. Members '[State Builder] r => Sem r ()
hardline' = append' P.hardline

line' :: forall r. Members '[State Builder] r => Sem r ()
line' = append' P.line

-- | It prints all remaining comments
end' :: forall r. Members '[State Builder] r => Sem r ()
end' = do
  cs <- gets (^. builderComments)
  forM_ cs printSpaceSpan
  modify' (set builderComments [])

printSpaceSpan :: forall r. Members '[State Builder] r => SpaceSpan -> Sem r ()
printSpaceSpan = mapM_ printSpaceSection . (^. spaceSpan)
  where
    printSpaceSection :: SpaceSection -> Sem r ()
    printSpaceSection = \case
      SpaceComment c -> printComment c
      SpaceLines {} -> line'

printComment :: Members '[State Builder] r => Comment -> Sem r ()
printComment c = do
  append' (annotate AnnComment (P.pretty c))
  hardline'

printCommentsUntil' :: forall r. Members '[State Builder] r => Interval -> Sem r (Maybe SpaceSpan)
printCommentsUntil' loc = do
  forceLine <- popEnsureLine
  g <- fmap sconcat . nonEmpty <$> whileJustM popSpaceSpan
  let noSpaceLines = fromMaybe True $ do
        g' <- (^. spaceSpan) <$> g
        return (not (any (has _SpaceLines) g'))
  when (forceLine && noSpaceLines) line'
  whenJust g printSpaceSpan
  return g
  where
    cmp :: SpaceSpan -> Bool
    cmp c = getLoc c ^. intervalStart < loc ^. intervalStart

    popEnsureLine :: Sem r Bool
    popEnsureLine = do
      b <- gets (^. builderEnsureEmptyLine)
      modify' (set builderEnsureEmptyLine False)
      return b

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
