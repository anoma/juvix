module Juvix.Compiler.Internal.Pretty
  ( module Juvix.Compiler.Internal.Pretty,
    module Juvix.Compiler.Internal.Pretty.Options,
  )
where

import Juvix.Compiler.Internal.Pretty.Ann
import Juvix.Compiler.Internal.Pretty.Ansi qualified as Ansi
import Juvix.Compiler.Internal.Pretty.Base
import Juvix.Compiler.Internal.Pretty.Options
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Prettyprinter.Render.Terminal qualified as Ansi

newtype PPOutput = PPOutput (Doc Ann)

ppOutDefault :: PrettyCode c => c -> AnsiText
ppOutDefault = AnsiText . PPOutput . doc defaultOptions

ppOut :: PrettyCode c => Options -> c -> AnsiText
ppOut o = AnsiText . PPOutput . doc o

ppTrace :: PrettyCode c => c -> Text
ppTrace = Ansi.renderStrict . reAnnotateS Ansi.stylize . layoutPretty defaultLayoutOptions . doc defaultOptions

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS Ansi.stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate Ansi.stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)
