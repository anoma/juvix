module Juvix.Compiler.Backend.Lean.Pretty where

import Juvix.Compiler.Backend.Lean.Pretty.Base
import Juvix.Compiler.Backend.Lean.Pretty.Options
import Juvix.Data.PPOutput
import Juvix.Prelude
import Prettyprinter.Render.Terminal qualified as Ansi

-- Generate ANSI-styled output for Lean code
ppOutDefault :: (PrettyCode c) => [Comment] -> c -> AnsiText
ppOutDefault comments =
  mkAnsiText
    . PPOutput
    . doc defaultOptions comments

-- Render Lean code for tracing (no styling)
ppTrace' :: (CanonicalProjection a Options, PrettyCode c) => a -> c -> Text
ppTrace' opts =
  Ansi.renderStrict
    . reAnnotateS stylize
    . layoutPretty defaultLayoutOptions
    . doc (project opts) []

ppTrace :: (PrettyCode c) => c -> Text
ppTrace = ppTrace' traceOptions

-- Render Lean code with comments as plain text
ppPrint :: (PrettyCode c) => [Comment] -> c -> Text
ppPrint comments = show . ppOutDefault comments
