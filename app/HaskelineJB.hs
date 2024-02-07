module HaskelineJB where

import Control.Monad.Trans.Reader
import GHC.IORef
import Juvix.Prelude.DarkArts
import System.Console.Haskeline
import System.Console.Repline

type InputTArg m a =
  ReaderT
    RunTerm
    ( ReaderT
        (IORef History)
        ( ReaderT
            (IORef KillRing)
            ( ReaderT
                Prefs
                (ReaderT (Settings m) m)
            )
        )
    )
    a

type KillRing = $(importHiddenConT "haskeline" "System.Console.Haskeline.Command.KillRing" "KillRing")

type RunTerm = $(importHiddenConT "haskeline" "System.Console.Haskeline.Term" "RunTerm")

type History = $(importHiddenConT "haskeline" "System.Console.Haskeline.History" "History")

unInputT :: InputT m a -> InputTArg m a
unInputT = $(importHiddenField "InputT" "haskeline" "System.Console.Haskeline.InputT" "unInputT")

mkInputT :: InputTArg m a -> InputT m a
mkInputT = $(importHiddenCon "haskeline" "System.Console.Haskeline.InputT" "InputT")

unHaskelineT :: HaskelineT m a -> InputT m a
unHaskelineT = $(importHiddenField "HaskelineT" "repline" "System.Console.Repline" "unHaskeline")

mkHaskelineT :: InputT m a -> HaskelineT m a
mkHaskelineT = $(importHiddenCon "repline" "System.Console.Repline" "HaskelineT")
