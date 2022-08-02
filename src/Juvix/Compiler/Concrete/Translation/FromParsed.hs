module Juvix.Compiler.Concrete.Translation.FromParsed
  ( module Juvix.Compiler.Concrete.Translation.FromParsed,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context,
  )
where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parsed
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

fromParsed ::
  Members '[Error JuvixError, Files, NameIdGen] r =>
  Parsed.ParserResult ->
  Sem r ScoperResult
fromParsed pr = mapError (JuvixError @ScoperError) $ do
  let root = pr ^. Parser.resultEntry . entryPointRoot
      modules = pr ^. Parser.resultModules
  scopeCheck pr root modules
