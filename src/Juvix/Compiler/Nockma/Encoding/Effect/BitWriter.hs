module Juvix.Compiler.Nockma.Encoding.Effect.BitWriter where

import Data.Bit as Bit
import Juvix.Prelude.Base
import VectorBuilder.Builder as Builder
import VectorBuilder.Vector

data BitWriter :: Effect where
  WriteBit :: Bit -> BitWriter m ()
  GetCurrentPosition :: BitWriter m Int

makeSem ''BitWriter

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
  WriteBit b -> writeBit' b
  GetCurrentPosition -> getCurrentPosition'

writeBit' :: (Member (State WriterState) r) => Bit -> Sem r ()
writeBit' b = modify appendBit
  where
    appendBit :: WriterState -> WriterState
    appendBit = over writerStateBuilder (<> Builder.singleton b)

getCurrentPosition' :: (Member (State WriterState) r) => Sem r Int
getCurrentPosition' = Builder.size <$> gets (^. writerStateBuilder)
