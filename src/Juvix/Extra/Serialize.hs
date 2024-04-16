{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Extra.Serialize
  ( module Serial,
    saveToFile,
    loadFromFile,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Serialize as Serial
import Juvix.Data.Effect.Files
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude.Base
import Juvix.Prelude.Path

instance Serialize (Path Abs File)

instance Serialize (Path Abs Dir)

instance Serialize Text where
  put txt = Serial.put (unpack txt)

  get = pack <$> Serial.get

instance (Serialize a) => Serialize (NonEmpty a)

instance (Hashable k, Serialize k, Serialize a) => Serialize (HashMap k a) where
  put m = Serial.put (HashMap.toList m)

  get = HashMap.fromList <$> Serial.get

instance (Hashable a, Serialize a) => Serialize (HashSet a) where
  put s = Serial.put (HashSet.toList s)

  get = HashSet.fromList <$> Serial.get

saveToFile :: (Members '[Files, TaggedLock] r, Serialize a) => Path Abs File -> a -> Sem r ()
saveToFile file a = withTaggedLockDir (parent file) $ do
  ensureDir' (parent file)
  let bs = runPut (Serial.put a)
  writeFileBS file bs

loadFromFile :: forall a r. (Members '[Files, TaggedLock] r, Serialize a) => Path Abs File -> Sem r (Maybe a)
loadFromFile file = withTaggedLockDir (parent file) $ do
  ex <- fileExists' file
  if
      | ex -> deserialize <$> readFileBS' file
      | otherwise -> return Nothing
  where
    deserialize :: ByteString -> Maybe a
    deserialize bs = case runGet (Serial.get @a) bs of
      Left {} -> Nothing
      Right a -> Just a
