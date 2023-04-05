{-# LANGUAGE NamedFieldPuns #-}

module Sexp.Parser where

import Mari.Library
import qualified Data.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top =
  T.testGroup
    "sexp parser tests"
    [ staringParenSpaceDoesntEffectParser,
      endingParenSpaceDoesntEffectParser,
      uncidoeParserCorrectly,
      numbersParseCorrectly,
      doublesParseCorrectly,
      stringParseCorrectly
    ]

staringParenSpaceDoesntEffectParser :: T.TestTree
staringParenSpaceDoesntEffectParser =
  T.testCase
    "space after () works as expected"
    (Sexp.parse "(1 2 3)" T.@=? Sexp.parse " (       1 2 3)")

endingParenSpaceDoesntEffectParser :: T.TestTree
endingParenSpaceDoesntEffectParser =
  T.testCase
    "space after () works as expected"
    (Sexp.parse "(1 2 3)" T.@=? Sexp.parse " (1 2 3              )       ")

uncidoeParserCorrectly :: T.TestTree
uncidoeParserCorrectly =
  T.testCase
    "space after () works as expected"
    ("Foo" :| ["X:Y<Z", "A---B-C"] T.@=? atomName)
  where
    Right (Sexp.Atom Sexp.A {Sexp.atomName}) = Sexp.parse "Foo.X:Y<Z.A---B-C"

numbersParseCorrectly :: T.TestTree
numbersParseCorrectly =
  T.testCase
    "Numbers parse correctly"
    (Right (Sexp.Atom (Sexp.N 3 Nothing)) T.@=? Sexp.parse "3")

doublesParseCorrectly :: T.TestTree
doublesParseCorrectly =
  T.testCase
    "doubles parse correctly"
    (Right (Sexp.Atom (Sexp.D 3.4 Nothing)) T.@=? Sexp.parse "3.4")

stringParseCorrectly :: T.TestTree
stringParseCorrectly =
  T.testCase
    "String parse correctly"
    (Right (Sexp.Atom (Sexp.S "hi" Nothing)) T.@=? Sexp.parse "\"hi\"")
