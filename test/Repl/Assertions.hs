module Repl.Assertions where

import Base
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Language.Value qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core

assertNoJuvixError :: Either JuvixError a -> IO a
assertNoJuvixError = either (assertFailure . ("JuvixError: " <>) . unpack . renderTextDefault) return

assertPrettyCodeEqual :: (Core.PrettyCode a) => (a -> a -> Bool) -> a -> a -> Assertion
assertPrettyCodeEqual eq expected actual = unless (eq expected actual) (assertFailure (unpack msg))
  where
    msg :: Text
    msg = "expected: " <> Core.ppTrace expected <> "\n but got: " <> Core.ppTrace actual

assertNodeEqual :: Core.Node -> Core.Node -> Assertion
assertNodeEqual = assertPrettyCodeEqual (==)

assertValueEqual :: Core.Value -> Core.Value -> Assertion
assertValueEqual = assertPrettyCodeEqual valueEq

valueEqList :: [Core.Value] -> [Core.Value] -> Bool
valueEqList vs1 vs2 = case (vs1, vs2) of
  ([], []) -> True
  ([], _) -> False
  (_, []) -> False
  (x : xs, y : ys) -> valueEq x y && valueEqList xs ys

valueEq :: Core.Value -> Core.Value -> Bool
valueEq n = \case
  Core.ValueConstrApp app1 -> case n of
    Core.ValueConstrApp app2 ->
      app1 ^. Core.constrAppName == app2 ^. Core.constrAppName
        && valueEqList (app1 ^. Core.constrAppArgs) (app2 ^. Core.constrAppArgs)
    _ -> False
  Core.ValueConstant v1 -> case n of
    Core.ValueConstant v2 -> v1 == v2
    _ -> False
  Core.ValueWildcard -> case n of
    Core.ValueWildcard -> True
    _ -> False
  Core.ValueFun -> case n of
    Core.ValueFun -> True
    _ -> False
  Core.ValueType -> case n of
    Core.ValueType -> True
    _ -> False
