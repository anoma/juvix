{-# LANGUAGE ApplicativeDo #-}

module CLI
  ( module CLI,
    module GlobalOptions,
    module Command,
  )
where

import Command
import GlobalOptions
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative
import Options.Applicative.Help.Pretty

data CLI = CLI
  { _cliGlobalOptions :: GlobalOptions,
    _cliCommand :: Command
  }

makeLenses ''CLI

parseCLI :: Parser CLI
parseCLI = do
  _cliGlobalOptions <- parseGlobalOptions
  _cliCommand <- parseCommand
  pure CLI {..}

cliMainFile :: CLI -> Maybe FilePath
cliMainFile = aux . (^. cliCommand)
  where
    aux :: Command -> Maybe FilePath
    aux = \case
      Scope s -> Just (head (s ^. scopeInputFiles))
      Parse s -> Just (s ^. parseInputFile)
      Termination (Calls s) -> Just (s ^. callsInputFile)
      Termination (CallGraph s) -> Just (s ^. graphInputFile)
      Html s -> Just (s ^. htmlInputFile)
      MiniHaskell s -> Just (s ^. miniHaskellInputFile)
      Highlight s -> Just (s ^. highlightInputFile)
      MiniC s -> Just (s ^. miniCInputFile)
      Compile s -> Just (s ^. compileInputFile)
      MicroJuvix (TypeCheck s) -> Just (s ^. microJuvixTypeInputFile)
      MicroJuvix (Pretty s) -> Just (s ^. microJuvixPrettyInputFile)
      MonoJuvix s -> Just (s ^. monoJuvixInputFile)
      DisplayVersion -> Nothing
      DisplayRoot -> Nothing

makeAbsPaths :: CLI -> IO CLI
makeAbsPaths = traverseOf cliCommand aux
  where
    aux :: Command -> IO Command
    aux = \case
      Scope s -> Scope <$> traverseOf scopeInputFiles (mapM makeAbsolute) s
      Parse s -> Parse <$> traverseOf parseInputFile makeAbsolute s
      Termination (Calls s) -> Termination . Calls <$> traverseOf callsInputFile makeAbsolute s
      Termination (CallGraph s) -> Termination . CallGraph <$> traverseOf graphInputFile makeAbsolute s
      Html s -> Html <$> traverseOf htmlInputFile makeAbsolute s
      MiniHaskell s -> MiniHaskell <$> traverseOf miniHaskellInputFile makeAbsolute s
      Highlight s -> Highlight <$> traverseOf highlightInputFile makeAbsolute s
      MiniC s -> MiniC <$> traverseOf miniCInputFile makeAbsolute s
      Compile s -> Compile <$> traverseOf compileInputFile makeAbsolute s
      MicroJuvix (TypeCheck s) -> MicroJuvix . TypeCheck <$> traverseOf microJuvixTypeInputFile makeAbsolute s
      MicroJuvix (Pretty s) -> MicroJuvix . Pretty <$> traverseOf microJuvixPrettyInputFile makeAbsolute s
      MonoJuvix s -> MonoJuvix <$> traverseOf monoJuvixInputFile makeAbsolute s
      DisplayVersion -> return DisplayVersion
      DisplayRoot -> return DisplayRoot

descr :: ParserInfo CLI
descr =
  info
    (parseCLI <**> helper)
    ( fullDesc
        <> progDesc "The MiniJuvix compiler."
        <> headerDoc (Just headDoc)
        <> footerDoc (Just foot)
    )
  where
    headDoc :: Doc
    headDoc = dullblue $ bold $ underline "MiniJuvix help"

    foot :: Doc
    foot = bold "maintainers: " <> "The MiniJuvix Team"
