module Commands.Extra where

import Juvix.Prelude hiding (Doc)
import Options.Applicative
import Options.Applicative.Builder.Internal
import Options.Applicative.Types

parserInputFile :: Parser FilePath
parserInputFile =
  argument
    str
    ( metavar "JUVIX_FILE"
        <> help "Path to a .juvix file"
        <> action "file"
    )

parserInputFiles :: Parser [FilePath]
parserInputFiles = many parserInputFile

addParser :: forall a c. Monoid a => Parser a -> Parser c -> Parser (a, c)
addParser parser = \case
  (NilP p) -> NilP ((\c -> (mempty, c)) <$> p)
  (OptP (Option (CmdReader n cs g) ps)) ->
    OptP (Option (CmdReader n cs (fmap optsInfo . g)) ps)
    where
      optsInfo :: ParserInfo b -> ParserInfo (a, b)
      optsInfo pInfo = pInfo {infoParser = newParser}
        where
          newParser = do
            opts <- parser
            rest <- infoParser pInfo
            pure (opts, rest)
  (OptP o) -> OptP ((mempty,) <$> o)
  (AltP p1 p2) -> AltP (addParser parser p1) (addParser parser p2)
  (MultP p1 p2) ->
    MultP
      ((\(g2, f) (g1, x) -> (g1 <> g2, f x)) <$> addParser parser p1)
      (addParser parser p2)
  (BindP p k) -> BindP (addParser parser p) $ \(g1, x) ->
    BindP (addParser parser $ k x) $ \(g2, x') ->
      pure (g1 <> g2, x')

hidden :: Bool -> Mod f a
hidden sure = optionMod $ \p ->
  if
      | not sure -> p
      | otherwise -> p {propVisibility = min Hidden (propVisibility p)}
