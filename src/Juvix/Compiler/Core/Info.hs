-- | This file defines Infos stored in JuvixCore Nodes. The Info data structure
--  maps an info type to an info of that type.
module Juvix.Compiler.Core.Info where

import Data.Dynamic
import Data.HashMap.Strict qualified as HashMap
import Juvix.Prelude hiding (insert)

class (Typeable a) => IsInfo a

newtype Info = Info
  { _infoMap :: HashMap TypeRep Dynamic
  }
  deriving newtype (Semigroup, Monoid)

-- | NOTE the NFData instance for Info is a noop. I don't think it's possible to
-- provide an NFData instance for it because a Dynamic can't have an instance.
-- Still, having this instance is useful so we can derive NFData for Node.
instance NFData Info where
  rnf _info = ()

type Key = Proxy

makeLenses ''Info

empty :: Info
empty = Info HashMap.empty

singleton :: forall a. (IsInfo a) => a -> Info
singleton a = insert a mempty

member :: forall a. (IsInfo a) => Key a -> Info -> Bool
member k i = HashMap.member (typeRep k) (i ^. infoMap)

lookup :: (IsInfo a) => Key a -> Info -> Maybe a
lookup k i = case HashMap.lookup (typeRep k) (i ^. infoMap) of
  Just a -> Just $ fromDyn a impossible
  Nothing -> Nothing

lookupDefault :: (IsInfo a) => a -> Info -> a
lookupDefault a i =
  fromDyn (HashMap.lookupDefault (toDyn a) (typeOf a) (i ^. infoMap)) impossible

lookup' :: (IsInfo a) => Key a -> Info -> a
lookup' k i = fromMaybe impossible (Juvix.Compiler.Core.Info.lookup k i)

(!) :: (IsInfo a) => Key a -> Info -> a
(!) = lookup'

insert :: (IsInfo a) => a -> Info -> Info
insert a i = Info (HashMap.insert (typeOf a) (toDyn a) (i ^. infoMap))

insertWith :: (IsInfo a) => (a -> a -> a) -> a -> Info -> Info
insertWith f a i = Info (HashMap.insertWith f' (typeOf a) (toDyn a) (i ^. infoMap))
  where
    f' x1 x2 = toDyn (f (fromDyn x1 impossible) (fromDyn x2 impossible))

delete :: (IsInfo a) => Key a -> Info -> Info
delete k i = Info (HashMap.delete (typeRep k) (i ^. infoMap))

adjust :: forall a. (IsInfo a) => (a -> a) -> Info -> Info
adjust f i =
  Info $
    HashMap.adjust
      (\x -> toDyn $ f $ fromDyn x impossible)
      (typeRep (Proxy :: Proxy a))
      (i ^. infoMap)

update :: forall a. (IsInfo a) => (a -> Maybe a) -> Info -> Info
update f i = Info (HashMap.update f' (typeRep (Proxy :: Proxy a)) (i ^. infoMap))
  where
    f' x = case f (fromDyn x impossible) of
      Just y -> Just (toDyn y)
      Nothing -> Nothing

alter :: forall a. (IsInfo a) => (Maybe a -> Maybe a) -> Info -> Info
alter f i = Info (HashMap.alter f' (typeRep (Proxy :: Proxy a)) (i ^. infoMap))
  where
    f' x = case y of
      Just y' -> Just (toDyn y')
      Nothing -> Nothing
      where
        y = case x of
          Just x' -> f (fromDyn x' impossible)
          Nothing -> f Nothing
