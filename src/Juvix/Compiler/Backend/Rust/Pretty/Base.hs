module Juvix.Compiler.Backend.Rust.Pretty.Base where

import Juvix.Compiler.Backend.Rust.Language
import Juvix.Compiler.Backend.Rust.Pretty.Keywords
import Juvix.Compiler.Backend.Rust.Pretty.Options
import Juvix.Data.CodeAnn

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts x =
  run $
    runReader opts $
      ppCode x

ppName :: NameKind -> Text -> Sem r (Doc Ann)
ppName k n = return $ annotate (AnnKind k) (pretty n)
