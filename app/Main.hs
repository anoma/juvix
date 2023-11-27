{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import App
import CommonOptions
import Data.String.Interpolate (i)
import GlobalOptions
import Juvix.Compiler.Pipeline.Root
import Juvix.Data.Effect.TaggedLock
import TopCommand
import TopCommand.Options

main :: IO ()
main = do
  let parserPreferences = prefs showHelpOnEmpty
  invokeDir <- getCurrentDir
  (_runAppIOArgsGlobalOptions, cli) <- customExecParser parserPreferences descr
  mbuildDir <- mapM (prepathToAbsDir invokeDir) (_runAppIOArgsGlobalOptions ^? globalBuildDir . _Just . pathPath)
  mainFile <- topCommandInputPath cli
  mapM_ checkMainFile mainFile
  _runAppIOArgsRoot <- findRootAndChangeDir LockModePermissive (containingDir <$> mainFile) mbuildDir invokeDir
  runFinal
    . resourceToIOFinal
    . embedToFinal @IO
    . runAppIO RunAppIOArgs {..}
    $ runTopCommand cli
  where
    checkMainFile :: SomePath b -> IO ()
    checkMainFile p = unlessM (doesSomePathExist p) err
      where
        err :: IO ()
        err = do
          hPutStrLn stderr [i|The input path #{p} does not exist|]
          exitFailure
