module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty,
    module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty.Ann,
  )
where

import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty.Ann
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty.Ansi qualified as Ansi
import Juvix.Prelude
import Juvix.Prelude.Pretty

newtype PPOutput = PPOutput (Doc Eann)

prettyError :: Doc Eann -> AnsiText
prettyError = AnsiText . PPOutput

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS Ansi.stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate Ansi.stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)

indent' :: Doc ann -> Doc ann
indent' = indent 2

prettyT :: Text -> Doc Eann
prettyT = pretty

highlight :: Doc Eann -> Doc Eann
highlight = annotate Highlight
