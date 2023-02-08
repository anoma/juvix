module Juvix.Compiler.Backend.Geb.Translation
  ( module Juvix.Compiler.Backend.Geb.Translation,
    module Juvix.Compiler.Backend.Geb.Translation.FromCore,
    module Juvix.Compiler.Backend.Geb.Translation.FromSource,
  )
where

import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty
import Juvix.Compiler.Backend.Geb.Translation.FromCore hiding (Env)
import Juvix.Compiler.Backend.Geb.Translation.FromSource

newtype Result = Result
  { _resultCode :: Text
  }

data LispPackageSpec = LispPackageSpec
  { _lispPackageName :: Text,
    _lispPackageEntry :: Text
  }

data ResultSpec
  = OnlyTerm
  | LispPackage LispPackageSpec

toResult :: ResultSpec -> Morphism -> Object -> Result
toResult spec morph obj = case spec of
  OnlyTerm ->
    Result (ppPrint morph <> "\n")
  LispPackage LispPackageSpec {..} ->
    Result (ppPrintLisp _lispPackageName _lispPackageEntry morph obj <> "\n")

makeLenses ''Result
makeLenses ''LispPackageSpec
