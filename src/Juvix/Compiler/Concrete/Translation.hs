module Juvix.Compiler.Concrete.Translation where

import Juvix.Compiler.Concrete.Data.Highlight.Input (HighlightBuilder)
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

type JudocStash = State (Maybe (Judoc 'Parsed))

fromSource ::
  (Members '[HighlightBuilder, Files, Error JuvixError, NameIdGen, Reader EntryPoint, PathResolver, Parser.PragmasStash] r) =>
  EntryPoint ->
  Sem r Scoper.ScoperResult
fromSource = Parser.fromSource >=> Scoper.fromParsed
