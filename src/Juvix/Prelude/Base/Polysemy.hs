module Juvix.Prelude.Base.Polysemy
  ( module Juvix.Prelude.Base.Polysemy,
    module Polysemy,
    module Polysemy.Error,
    module Polysemy.Input,
    module Polysemy.Fixpoint,
    module Polysemy.Output,
    module Polysemy.Reader,
    module Polysemy.Tagged,
    module Polysemy.Resource,
    module Polysemy.Scoped,
    module Polysemy.State,
  )
where

import Data.Stream qualified as Stream
import Juvix.Prelude.Base.Foundation
import Polysemy hiding (embed)
import Polysemy.Embed qualified as Embed
import Polysemy.Error hiding (fromEither)
import Polysemy.Fixpoint
import Polysemy.Input
import Polysemy.Output
import Polysemy.Reader
import Polysemy.Resource
import Polysemy.Scoped
import Polysemy.State
import Polysemy.Tagged hiding (tag)

type EmbedIO = Embed.Embed IO

embed :: (Member EmbedIO r) => IO a -> Sem r a
embed = Embed.embed

mapReader :: (Member (Reader e1) r) => (e1 -> e2) -> Sem (Reader e2 ': r) a -> Sem r a
mapReader f s = do
  e <- ask
  runReader (f e) s

execOutputList :: Sem (Output o ': r) a -> Sem r [o]
execOutputList = fmap fst . runOutputList

runInputInfinite :: Stream i -> Sem (Input i ': r) a -> Sem r a
runInputInfinite s =
  evalState s
    . reinterpret
      ( \case
          Input -> do
            Stream.Cons i is <- get
            put is
            return i
      )
