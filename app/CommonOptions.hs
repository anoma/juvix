-- | Contains common options reused in several commands.
module CommonOptions
  ( module CommonOptions,
    module Juvix.Prelude,
    module Options.Applicative,
  )
where

import Control.Exception qualified as GHC
import Juvix.Compiler.Core.Data.TransformationId.Parser
import Juvix.Prelude
import Options.Applicative
import System.Process
import Text.Read (readMaybe)
import Prelude (show)

-- | Paths that are input are used to detect the root of the project.
data AppPath f = AppPath
  { _pathPath :: SomeBase f,
    _pathIsInput :: Bool
  }
  deriving stock (Data, Eq)

makeLenses ''AppPath

instance Show (AppPath f) where
  show = Prelude.show . (^. pathPath)

parseInputJuvixFile :: Parser (AppPath File)
parseInputJuvixFile = do
  _pathPath <-
    argument
      someFileOpt
      ( metavar "JUVIX_FILE"
          <> help "Path to a .juvix file"
          <> completer juvixCompleter
      )
  pure AppPath {_pathIsInput = True, ..}

parseInputJuvixCoreFile :: Parser (AppPath File)
parseInputJuvixCoreFile = do
  _pathPath <-
    argument
      someFileOpt
      ( metavar "JUVIX_CORE_FILE"
          <> help "Path to a .jvc file"
          <> completer juvixCoreCompleter
      )
  pure AppPath {_pathIsInput = True, ..}

parseInputJuvixGebFile :: Parser (AppPath File)
parseInputJuvixGebFile = do
  _pathPath <-
    argument
      someFileOpt
      ( metavar "JUVIX_GEB_FILE"
          <> help "Path to a .geb or custom .lisp file"
          <> completer juvixGebCompleter
          <> completer juvixGebLispCompleter
      )
  pure AppPath {_pathIsInput = True, ..}

parseInputJuvixAsmFile :: Parser (AppPath File)
parseInputJuvixAsmFile = do
  _pathPath <-
    argument
      someFileOpt
      ( metavar "JUVIX_ASM_FILE"
          <> help "Path to a .jva file"
          <> completer juvixAsmCompleter
      )
  pure AppPath {_pathIsInput = True, ..}

parseInputCFile :: Parser (AppPath File)
parseInputCFile = do
  _pathPath <-
    argument
      someFileOpt
      ( metavar "C_FILE"
          <> help "Path to a .c file"
          <> completer juvixCCompleter
      )
  pure AppPath {_pathIsInput = True, ..}

parseGenericInputFile :: Parser (AppPath File)
parseGenericInputFile = do
  _pathPath <-
    argument
      someFileOpt
      ( metavar "INPUT_FILE"
          <> help "Path to input file"
          <> action "file"
      )
  pure AppPath {_pathIsInput = True, ..}

parseGenericOutputFile :: Parser (AppPath File)
parseGenericOutputFile = do
  _pathPath <-
    option
      someFileOpt
      ( long "output"
          <> short 'o'
          <> metavar "OUTPUT_FILE"
          <> help "Path to output file"
          <> action "file"
      )
  pure AppPath {_pathIsInput = False, ..}

parseGenericOutputDir :: Mod OptionFields (SomeBase Dir) -> Parser (AppPath Dir)
parseGenericOutputDir m = do
  _pathPath <-
    option
      someDirOpt
      ( long "output-dir"
          <> metavar "OUTPUT_DIR"
          <> help "Path to output directory"
          <> action "directory"
          <> m
      )
  pure AppPath {_pathIsInput = False, ..}

someFileOpt :: ReadM (SomeBase File)
someFileOpt = eitherReader aux
  where
    aux :: String -> Either String (SomeBase File)
    aux s = maybe (Left $ s <> " is not a file path") Right (parseSomeFile s)

someDirOpt :: ReadM (SomeBase Dir)
someDirOpt = eitherReader aux
  where
    aux :: String -> Either String (SomeBase Dir)
    aux s = maybe (Left $ s <> " is not a directory path") Right (parseSomeDir s)

naturalNumberOpt :: ReadM Word
naturalNumberOpt = eitherReader aux
  where
    aux :: String -> Either String Word
    aux s = maybe (Left $ s <> " is not a nonnegative number") Right (readMaybe s :: Maybe Word)

extCompleter :: String -> Completer
extCompleter ext = mkCompleter $ \word -> do
  let cmd = unwords ["compgen", "-o", "plusdirs", "-f", "-X", "!*." <> ext, "--", requote word]
  result <- GHC.try @GHC.SomeException $ readProcess "bash" ["-c", cmd] ""
  return . lines . fromRight [] $ result

juvixCompleter :: Completer
juvixCompleter = extCompleter "juvix"

juvixCoreCompleter :: Completer
juvixCoreCompleter = extCompleter "jvc"

juvixGebLispCompleter :: Completer
juvixGebLispCompleter = extCompleter "lisp"

juvixGebCompleter :: Completer
juvixGebCompleter = extCompleter "geb"

juvixAsmCompleter :: Completer
juvixAsmCompleter = extCompleter "jva"

juvixCCompleter :: Completer
juvixCCompleter = extCompleter "c"

requote :: String -> String
requote s =
  let -- Bash doesn't appear to allow "mixed" escaping
      -- in bash completions. So we don't have to really
      -- worry about people swapping between strong and
      -- weak quotes.
      unescaped =
        case s of
          -- It's already strongly quoted, so we
          -- can use it mostly as is, but we must
          -- ensure it's closed off at the end and
          -- there's no single quotes in the
          -- middle which might confuse bash.
          ('\'' : rs) -> unescapeN rs
          -- We're weakly quoted.
          ('"' : rs) -> unescapeD rs
          -- We're not quoted at all.
          -- We need to unescape some characters like
          -- spaces and quotation marks.
          elsewise -> unescapeU elsewise
   in strong unescaped
  where
    strong :: String -> String
    strong ss = '\'' : foldr go "'" ss
      where
        -- If there's a single quote inside the
        -- command: exit from the strong quote and
        -- emit it the quote escaped, then resume.
        go '\'' t = "'\\''" ++ t
        go h t = h : t

    -- Unescape a strongly quoted string
    -- We have two recursive functions, as we
    -- can enter and exit the strong escaping.
    unescapeN = goX
      where
        goX ('\'' : xs) = goN xs
        goX (x : xs) = x : goX xs
        goX [] = []

        goN ('\\' : '\'' : xs) = '\'' : goN xs
        goN ('\'' : xs) = goX xs
        goN (x : xs) = x : goN xs
        goN [] = []

    -- Unescape an unquoted string
    unescapeU = goX
      where
        goX [] = []
        goX ('\\' : x : xs) = x : goX xs
        goX (x : xs) = x : goX xs

    -- Unescape a weakly quoted string
    unescapeD = goX
      where
        -- Reached an escape character
        goX ('\\' : x : xs)
          -- If it's true escapable, strip the
          -- slashes, as we're going to strong
          -- escape instead.
          | x `elem` ("$`\"\\\n" :: String) = x : goX xs
          | otherwise = '\\' : x : goX xs
        -- We've ended quoted section, so we
        -- don't recurse on goX, it's done.
        goX ('"' : xs) =
          xs
        -- Not done, but not a special character
        -- just continue the fold.
        goX (x : xs) =
          x : goX xs
        goX [] =
          []

optDeBruijn :: Parser Bool
optDeBruijn =
  switch
    ( long "show-de-bruijn"
        <> help "Show variable de Bruijn indices"
    )

optIdentIds :: Parser Bool
optIdentIds =
  switch
    ( long "show-ident-ids"
        <> help "Show identifier IDs"
    )

optNoDisambiguate :: Parser Bool
optNoDisambiguate =
  switch
    ( long "no-disambiguate"
        <> help "Don't disambiguate the names of bound variables"
    )

optTransformationIds :: Parser [TransformationId]
optTransformationIds =
  option
    (eitherReader parseTransf)
    ( long "transforms"
        <> short 't'
        <> value []
        <> metavar "[Transform]"
        <> completer (mkCompleter (return . completionsString))
        <> help "hint: use autocomplete"
    )
  where
    parseTransf :: String -> Either String [TransformationId]
    parseTransf = mapLeft unpack . parseTransformations . pack
