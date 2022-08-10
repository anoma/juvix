module Juvix.Data.PPOutput
  ( module Juvix.Data.PPOutput,
    module Juvix.Data.CodeAnn,
  )
where

import Juvix.Data.CodeAnn
import Juvix.Prelude.Base

newtype PPOutput = PPOutput (Doc Ann)

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)

ppOutput :: Doc Ann -> AnsiText
ppOutput = AnsiText . PPOutput
