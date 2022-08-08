module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty,
    module Juvix.Data.CodeAnn,
  )
where

import Juvix.Data.CodeAnn
import Juvix.Prelude
import Juvix.Prelude.Pretty

newtype PPOutput = PPOutput (Doc Ann)

prettyError :: Doc Ann -> AnsiText
prettyError = AnsiText . PPOutput

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)

indent' :: Doc ann -> Doc ann
indent' = indent 2

prettyT :: Text -> Doc Ann
prettyT = pretty

highlight :: Doc Ann -> Doc Ann
highlight = annotate AnnCode
