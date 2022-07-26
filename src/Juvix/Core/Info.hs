{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Info where

import Juvix.Core.GNode
import Juvix.Core.Location
import Juvix.Core.Name
import Juvix.Core.Type
import Juvix.Prelude

data Info i = Info
  { _infoName :: Maybe Name,
    _infoType :: Maybe Type,
    _infoLoc :: Maybe Location,
    _infoBinding :: Maybe BindingInfo,
    _infoMore :: i
    -- much more will be added here by further phases of the pipeline (program
    -- transformations on Core)
  }

data BindingInfo = BindingInfo
  { _bindingName :: Name,
    _bindingType :: Type
  }

makeLenses ''Info
makeLenses ''BindingInfo

type Node' i = GNode (Info i)

data NoInfo = NoInfo

type Node = Node' NoInfo

instance GNodeFunctor i j => GNodeFunctor (Info i) j where
  nmapM :: Monad m => (GNode j -> m (GNode j)) -> Info i -> m (Info i)
  nmapM f i = do
    i' <- nmapM f (i ^. infoMore)
    return i {_infoMore = i'}

instance GNodeFoldable i j => GNodeFoldable (Info i) j where
  nfoldM :: Monad m => (a -> a -> a) -> m a -> (GNode j -> m a) -> Info i -> m a
  nfoldM uplus a f i = nfoldM uplus a f (i ^. infoMore)

instance GNodeEmpty i => GNodeEmpty (Info i) where
  iempty :: Info i
  iempty =
    Info
      { _infoName = Nothing,
        _infoType = Nothing,
        _infoLoc = Nothing,
        _infoBinding = Nothing,
        _infoMore = iempty
      }

instance
  ( GNodeEmpty i,
    GNodeFunctor i (Info i),
    GNodeFoldable i (Info i)
  ) =>
  GNodeInfo (Info i)

instance GNodeFunctor NoInfo j where
  nmapM :: Monad m => (GNode j -> m (GNode j)) -> NoInfo -> m NoInfo
  nmapM _ i = return i

instance GNodeFoldable NoInfo j where
  nfoldM :: (a -> a -> a) -> m a -> (GNode j -> m a) -> NoInfo -> m a
  nfoldM _ a _ _ = a

instance GNodeEmpty NoInfo where
  iempty :: NoInfo
  iempty = NoInfo

testFun :: Node -> Node
testFun = removeClosures

hasNameInfo :: Info i -> Bool
hasNameInfo i = isJust (i ^. infoName)

getNameInfo :: Info i -> Name
getNameInfo i = fromMaybe impossible (i ^. infoName)

hasTypeInfo :: Info i -> Bool
hasTypeInfo i = isJust (i ^. infoType)

getTypeInfo :: Info i -> Type
getTypeInfo i = fromMaybe impossible (i ^. infoType)

hasLocInfo :: Info i -> Bool
hasLocInfo i = isJust (i ^. infoLoc)

getLocInfo :: Info i -> Location
getLocInfo i = fromMaybe impossible (i ^. infoLoc)

hasBindingInfo :: Info i -> Bool
hasBindingInfo i = isJust (i ^. infoBinding)

getBindingInfo :: Info i -> BindingInfo
getBindingInfo i = fromMaybe impossible (i ^. infoBinding)
