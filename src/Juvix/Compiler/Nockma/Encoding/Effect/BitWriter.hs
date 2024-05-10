module Juvix.Compiler.Nockma.Encoding.Effect.BitWriter where

import Data.Bit as Bit
import Juvix.Prelude.Base
import VectorBuilder.Builder as Builder
import VectorBuilder.Vector

data BitWriter :: Effect where
  WriteBuilder :: Builder Bit -> BitWriter m ()
  GetCurrentPosition :: BitWriter m Int

makeSem ''BitWriter

writeBit :: (Member BitWriter r) => Bit -> Sem r ()
writeBit b = writeBuilder (Builder.singleton b)

writeOne :: (Member BitWriter r) => Sem r ()
writeOne = writeBit (Bit True)

writeZero :: (Member BitWriter r) => Sem r ()
writeZero = writeBit (Bit False)

newtype WriterState = WriterState
  { _writerStateBuilder :: Builder Bit
  }

makeLenses ''WriterState

initWriterState :: WriterState
initWriterState = WriterState {_writerStateBuilder = mempty}

runBitWriter :: forall a r. Sem (BitWriter ': r) a -> Sem r (Bit.Vector Bit, a)
runBitWriter sem = do
  (s, res) <- runState initWriterState (re sem)
  return (build (s ^. writerStateBuilder), res)

execBitWriter :: forall a r. Sem (BitWriter ': r) a -> Sem r (Bit.Vector Bit)
execBitWriter sem = do
  s <- execState initWriterState (re sem)
  return (build (s ^. writerStateBuilder))

re :: Sem (BitWriter ': r) a -> Sem (State WriterState ': r) a
re = interpretTop $ \case
  WriteBuilder b -> writeBuilder' b
  GetCurrentPosition -> getCurrentPosition'

writeBuilder' :: (Member (State WriterState) r) => Builder Bit -> Sem r ()
writeBuilder' b = modify appendBuilder
  where
    appendBuilder :: WriterState -> WriterState
    appendBuilder = over writerStateBuilder (<> b)

getCurrentPosition' :: (Member (State WriterState) r) => Sem r Int
getCurrentPosition' = Builder.size <$> gets (^. writerStateBuilder)
