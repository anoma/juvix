module Commands.Dev.Geb.Repl.Format
  ( module Commands.Dev.Geb.Repl.Format,
    pretty,
  )
where

import Juvix.Prelude
import Juvix.Prelude.Pretty
import Prettyprinter.Render.Terminal

data ReplStyle
  = ReplPrompt
  | ReplName
  | ReplError
  | ReplIntro
  | ReplNormal

newtype ReplMessageDoc = ReplMessageDoc (Doc ReplStyle)

instance HasAnsiBackend ReplMessageDoc where
  toAnsiStream (ReplMessageDoc o) = reAnnotateS stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (ReplMessageDoc o) = reAnnotate stylize o

instance HasTextBackend ReplMessageDoc where
  toTextDoc (ReplMessageDoc o) = unAnnotate o
  toTextStream (ReplMessageDoc o) = layoutPretty defaultLayoutOptions (unAnnotate o)

stylize :: ReplStyle -> AnsiStyle
stylize = \case
  ReplPrompt -> color Blue
  ReplName -> color Green
  ReplError -> color Red
  ReplNormal -> color Blue
  ReplIntro -> bold

normal :: Text -> Doc ReplStyle
normal = annotate ReplNormal . pretty

ppOutput :: Doc ReplStyle -> AnsiText
ppOutput = AnsiText . ReplMessageDoc
