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

data Builder = Builder
  { -- | comments sorted by starting location
    _builderComments :: [SpaceSpan],
    -- | New elements are put at the front
    _builderQueue :: [Doc Ann],
    _builderDoc :: Doc Ann,
    _builderEnsureEmptyLine :: Bool,
    _builderEnd :: FileLoc
  }

data ExactPrint :: Effect where
  NoLoc :: Doc Ann -> ExactPrint m ()
  -- | Used to print parentheses after comments.
  Enqueue :: Doc Ann -> ExactPrint m ()
  PrintCommentsUntil :: Interval -> ExactPrint m (Maybe SpaceSpan)
  EnsureEmptyLine :: ExactPrint m ()
  Region :: (Doc Ann -> Doc Ann) -> m b -> ExactPrint m b
  -- | Both doc arguments are required to span the same region.
  RegionAlt :: (Doc Ann -> Doc Ann -> Doc Ann) -> (b -> b -> b) -> m b -> m b -> ExactPrint m b
  End :: ExactPrint m ()

makeSem ''ExactPrint

makeLenses ''Builder

initialBuilder :: Maybe FileComments -> Builder
initialBuilder cs =
  Builder
    { _builderComments = fromMaybe [] (cs ^? _Just . fileCommentsSorted),
      _builderDoc = mempty,
      _builderQueue = mempty,
      _builderEnsureEmptyLine = False,
      _builderEnd = FileLoc 0 0 0
    }

execExactPrint :: Maybe FileComments -> Sem (ExactPrint ': r) x -> Sem r (Doc Ann)
execExactPrint cs = fmap fst . runExactPrint cs

runExactPrint :: forall r a. Maybe FileComments -> Sem (ExactPrint ': r) a -> Sem r (Doc Ann, a)
runExactPrint cs = reinterpretH (runPrivateStateAsDoc (initialBuilder cs)) handler
  where
    runPrivateStateAsDoc ::
      forall b.
      Builder ->
      Sem (State Builder ': r) b ->
      Sem r (Doc Ann, b)
    runPrivateStateAsDoc b = fmap (first (^. builderDoc)) . runState b

    handler ::
      forall x (r' :: [Effect]) (localEs :: [Effect]).
      (Member ExactPrint localEs) =>
      LocalEnv localEs (State Builder ': r') ->
      ExactPrint (Sem localEs) x ->
      Sem (State Builder ': r') x
    handler locEnv = \case
      NoLoc p -> noLoc' p
      EnsureEmptyLine -> modify' (set builderEnsureEmptyLine True)
      End -> end'
      Enqueue d -> enqueue' d
      PrintCommentsUntil l -> printCommentsUntil' l
      Region regionModif (m :: Sem localEs x) -> do
        st0 :: Builder <- set builderDoc mempty <$> get
        let runner :: Sem (State Builder ': localEs) x -> Sem localEs (Builder, x)
            runner = runState st0

            helper :: (forall w. Sem localEs w -> Sem r' w) -> Sem r' (Builder, x)
            helper unlift = unlift (impose runner handler m)

            inner :: Sem r' (Builder, x)
            inner = localSeqUnliftCommon locEnv helper
        (st' :: Builder, fx) <- raise inner
        doc' <- gets (^. builderDoc)
        put
          Builder
            { _builderDoc = doc' <> regionModif (st' ^. builderDoc),
              _builderComments = st' ^. builderComments,
              _builderEnd = st' ^. builderEnd,
              _builderQueue = st' ^. builderQueue,
              _builderEnsureEmptyLine = st' ^. builderEnsureEmptyLine
            }
        return fx
      -- `m1` and `m2` are required to span the same region.
      RegionAlt regionModif alt (m1 :: Sem localEs x) (m2 :: Sem localEs x) -> do
        st0 :: Builder <- set builderDoc mempty <$> get
        let runner :: Sem (State Builder ': localEs) x -> Sem localEs (Builder, x)
            runner = runState st0

            helper :: Sem localEs x -> (forall w. Sem localEs w -> Sem r' w) -> Sem r' (Builder, x)
            helper m unlift = unlift (impose runner handler m)

            inner1 :: Sem r' (Builder, x)
            inner1 = localSeqUnliftCommon locEnv (helper m1)

            inner2 :: Sem r' (Builder, x)
            inner2 = localSeqUnliftCommon locEnv (helper m2)
        (st1 :: Builder, fx1) <- raise inner1
        (st2 :: Builder, fx2) <- raise inner2
        massert (length (st1 ^. builderComments) == length (st2 ^. builderComments))
        doc' <- gets (^. builderDoc)
        put
          Builder
            { _builderDoc = doc' <> regionModif (st1 ^. builderDoc) (st2 ^. builderDoc),
              _builderComments = st1 ^. builderComments,
              _builderEnd = st1 ^. builderEnd,
              _builderQueue = mempty,
              _builderEnsureEmptyLine = st1 ^. builderEnsureEmptyLine
            }
        return (alt fx1 fx2)

enqueue' :: forall r. (Members '[State Builder] r) => Doc Ann -> Sem r ()
enqueue' d = modify (over builderQueue (d :))

noLoc' :: forall r. (Members '[State Builder] r) => Doc Ann -> Sem r ()
noLoc' d = do
  popQueue
  append' d

append' :: forall r. (Members '[State Builder] r) => Doc Ann -> Sem r ()
append' d = modify (over builderDoc (<> d))

hardline' :: forall r. (Members '[State Builder] r) => Sem r ()
hardline' = append' P.hardline

line' :: forall r. (Members '[State Builder] r) => Sem r ()
line' = append' P.line

-- | It prints all remaining comments
end' :: forall r. (Members '[State Builder] r) => Sem r ()
end' = do
  cs <- gets (^. builderComments)
  forM_ cs printSpaceSpan
  modify' (set builderComments [])

printSpaceSpan :: forall r. (Members '[State Builder] r) => SpaceSpan -> Sem r ()
printSpaceSpan = mapM_ printSpaceSection . (^. spaceSpan)
  where
    printSpaceSection :: SpaceSection -> Sem r ()
    printSpaceSection = \case
      SpaceComment c -> printComment c
      SpaceLines {} -> line'

printComment :: (Members '[State Builder] r) => Comment -> Sem r ()
printComment c = do
  append' (ppCodeAnn c)
  hardline'

popQueue :: (Members '[State Builder] r) => Sem r ()
popQueue = do
  q <- gets (^. builderQueue)
  modify' (set builderQueue mempty)
  append' (mconcat (reverse q))

printCommentsUntil' :: forall r. (Members '[State Builder] r) => Interval -> Sem r (Maybe SpaceSpan)
printCommentsUntil' loc = do
  forceLine <- popEnsureLine
  g :: Maybe SpaceSpan <- fmap sconcat . nonEmpty <$> whileJustM popSpaceSpan
  let noSpaceLines = fromMaybe True $ do
        g' <- (^. spaceSpan) <$> g
        return (not (any (has _SpaceLines) g'))
  when (forceLine && noSpaceLines) line'
  whenJust g printSpaceSpan
  popQueue
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
