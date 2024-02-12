module Repl.Assertions where

import Base
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Language.Value qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core

assertNoJuvixError :: Either JuvixError a -> IO a
assertNoJuvixError = either (assertFailure . ("JuvixError: " <>) . unpack . renderTextDefault) return

assertPrettyCodeEqual :: (Core.PrettyCode a, Eq a) => a -> a -> Assertion
assertPrettyCodeEqual expected actual = unless (expected == actual) (assertFailure (unpack msg))
  where
    msg :: Text
    msg = "expected: " <> Core.ppTrace expected <> "\n but got: " <> Core.ppTrace actual

assertNodeEqual :: Core.Node -> Core.Node -> Assertion
assertNodeEqual = assertPrettyCodeEqual

assertValueEqual :: Core.Value -> Core.Value -> Assertion
assertValueEqual = assertPrettyCodeEqual
