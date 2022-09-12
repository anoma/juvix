-- | Contains common options reused in several commands.
module CommonOptions (
  module CommonOptions,
  module Juvix.Prelude,
  module Options.Applicative,
                     ) where

import Juvix.Prelude
import Control.Exception qualified as GHC
import Options.Applicative
import System.Process


newtype Path = Path {_unPath :: FilePath}
  deriving stock (Data)
makeLenses ''Path

instance IsString Path where
  fromString = Path

parseInputJuvixFile :: Parser Path
parseInputJuvixFile =
  argument
    str
    ( metavar "JUVIX_FILE"
        <> help "Path to a .juvix file"
        <> completer juvixCompleter
    )

juvixCompleter :: Completer
juvixCompleter = mkCompleter $ \word -> do
  let cmd = unwords ["compgen", "-o", "plusdirs", "-f", "-X", "!*.juvix", "--", requote word]
  result <- GHC.try @GHC.SomeException $ readProcess "bash" ["-c", cmd] ""
  return . lines . fromRight [] $ result

requote :: String -> String
requote s =
  let
    -- Bash doesn't appear to allow "mixed" escaping
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
        ('\'': rs) -> unescapeN rs

        -- We're weakly quoted.
        ('"': rs)  -> unescapeD rs

        -- We're not quoted at all.
        -- We need to unescape some characters like
        -- spaces and quotation marks.
        elsewise   -> unescapeU elsewise
  in
    strong unescaped

  where
    strong :: String -> String
    strong ss = '\'' : foldr go "'" ss
      where
        -- If there's a single quote inside the
        -- command: exit from the strong quote and
        -- emit it the quote escaped, then resume.
        go '\'' t = "'\\''" ++ t
        go h t    = h : t

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
        goX ('"' : xs)
          = xs
        -- Not done, but not a special character
        -- just continue the fold.
        goX (x : xs)
          = x : goX xs
        goX []
          = []

class HasPaths a where
  paths :: Traversal' a FilePath

optDeBruijn :: Parser Bool
optDeBruijn =
  switch
    ( long "show-de-bruijn"
        <> help "Show variable de Bruijn indices"
    )
