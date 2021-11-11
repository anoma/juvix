module MiniJuvix.Monad where


newtype MiniJuvixT e r s m a 
  = MiniJuvixT {unMgT :: E.ExceptT e (R.ReaderT r (St.StateT s m)) a}
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (MiniJuvixT e r s m) where
  liftIO = MiniJuvixT . liftIO


-- type MiniJuvix = MiniJuvixT () [Name] (S.Set Err) IO