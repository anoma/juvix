module MiniJuvix.Monad () where

--------------------------------------------------------------------------------

-- import MiniJuvix.Utils.Prelude

--------------------------------------------------------------------------------

-- newtype MiniJuvixT e r s m a
--   = MiniJuvixT {unMgT :: ExceptT e (ReaderT r (StateT s m)) a}
--   deriving anyclass (Functor, Applicative, Monad)

-- instance MonadIO m => MonadIO (MiniJuvixT e r s m) where
--   liftIO = MiniJuvixT . liftIO
-- -- type MiniJuvix = MiniJuvixT () [Name] (S.Set Err) IO

-- type MiniJuvix = MiniJuvixT () [Name] (Set Error) IO

-- runMiniJuvixT :: MiniJuvixT e r s m a -> r -> s -> m (Either e a, s)
-- runMiniJuvixT mgm r s =
--   (`St.runStateT` s) . (`R.runReaderT` r) . E.runExceptT $ unMgT mgm

-- runMiniJuvix :: MiniJuvix a -> IO (Either () a, S.Set Err)
-- runMiniJuvix m = runMiniJuvixT m [] S.empty

-- -- | Retrieves the state within a MiniJuvixT.
-- get :: Monad m => MiniJuvixT e r s m s
-- get = MiniJuvixT (lift (lift St.get))

-- -- | Modifies the state within a MiniJuvixT using the provided function.
-- modify :: Monad m => (s -> s) -> MiniJuvixT e r s m ()
-- modify f = MiniJuvixT (lift (lift (St.modify f)))

-- -- | Throws an exception within a MiniJuvixT.
-- throwE :: Monad m => e -> MiniJuvixT e s r m a
-- throwE = MiniJuvixT . E.throwE

-- -- | Catches an exception within a MiniJuvixT.
-- catchE ::
--   Monad m =>
--   MiniJuvixT e r s m a ->
--   (e -> MiniJuvixT e r s m a) ->
--   MiniJuvixT e r s m a
-- catchE me f = MiniJuvixT (unMgT me `E.catchE` (unMgT . f))

-- -- | Retrieves the environment within a MiniJuvixT.
-- ask :: Monad m => MiniJuvixT e r s m r
-- ask = MiniJuvixT (lift R.ask)
