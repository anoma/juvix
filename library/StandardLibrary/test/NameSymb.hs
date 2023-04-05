module NameSymb where

--------------------------------------------------------------------------------

import Mari.Library
import qualified Mari.Library.NameSymbol as NameSymbol
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Test.Tasty.QuickCheck as T

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Top Level Test
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "NameSymbol tests:"
    [idIsId, infixSymbolCase]

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

idL :: Symbol -> Symbol
idL = NameSymbol.toSymbol . NameSymbol.fromSymbol

idR :: NameSymbol.T -> NameSymbol.T
idR = NameSymbol.fromSymbol . NameSymbol.toSymbol

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

idIsId :: T.TestTree
idIsId =
  T.forAll (T.listOf T.arbitraryUnicodeChar) (appenDot . intern)
    |> T.testProperty "toSymbol and fromSymbol are inverses"

------------------
-- IdL subset
------------------

infixSymbolCase :: T.TestTree
infixSymbolCase =
  let str = "Foo.Bar._Foo_-_.-..->.Bar.(Foo)...-..>.."
   in T.testCase
        "infix functions are properly reserved"
        (idL str T.@=? str)

--------------------------------------------------------------------------------
-- property Helpers
--------------------------------------------------------------------------------

appenDot :: Symbol -> T.Property
appenDot symb =
  eq symb T..&&. eq dotEnd T..&&. eq dotMiddle T..&&. eq dotStart
  where
    eq s = idL s T.=== s
    --
    dotEnd = symb <> "."
    dotStart = "." <> symb
    dotMiddle = symb <> "." <> symb
