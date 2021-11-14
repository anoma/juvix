module MiniJuvix.Utils.Pretty
  ( Doc,
    Pretty (..),
    unicode,
    ascii,
    color,
    render,
    hardlines,
    format,
    annotateSpecialSymbol,
    printList,
  )
where

--------------------------------------------------------------------------------

import qualified Data.List as List
import MiniJuvix.Utils.Prelude
import Prettyprinter hiding
  ( Doc,
    Pretty (..),
  )
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as Term

--------------------------------------------------------------------------------
-- See https://hackage.haskell.org/package/prettyprinter-1.7.1/docs/Prettyprinter.html
--     https://hackage.haskell.org/package/prettyprinter-ansi-terminal
-- to know why we decide to have the def. below.

type Doc = PP.Doc Term.AnsiStyle

render :: PP.Doc Term.AnsiStyle -> Text
render = Term.renderStrict . layoutSmart defaultLayoutOptions

hardlines :: [PP.Doc Term.AnsiStyle] -> PP.Doc Term.AnsiStyle
hardlines = mconcat . intersperse hardline

--------------------------------------------------------------------------------
-- Styling
--------------------------------------------------------------------------------

data SpecialSymbol
  = UniverseTypeSymbol
  | PiTypeSymbol
  | LambdaSymbol
  | ArrowSymbol
  | TensorTypeSymbol
  | UnitTypeSymbol
  | UnitSymbol
  | SumTypeSymbol
  | InlSymbol
  | InrSymbol
  | ManyQuantitySymbol
  | ColonSymbol
  | DoubleColonSymbol

type Unicode = String

type ASCII = String

unicode :: SpecialSymbol -> Unicode
unicode UniverseTypeSymbol = "Ʉ"
unicode PiTypeSymbol = "Π"
unicode LambdaSymbol = "λ"
unicode ArrowSymbol = "→"
unicode TensorTypeSymbol = "⊗"
unicode UnitTypeSymbol = "𝟭"
unicode UnitSymbol = "*"
unicode SumTypeSymbol = "+"
unicode InlSymbol = "inl"
unicode InrSymbol = "inr"
unicode ManyQuantitySymbol = "ω"
unicode ColonSymbol = "∶"
unicode DoubleColonSymbol = "∷"

ascii :: SpecialSymbol -> ASCII
ascii UniverseTypeSymbol = "U"
ascii PiTypeSymbol = "Pi"
ascii LambdaSymbol = "\\"
ascii ArrowSymbol = "->"
ascii TensorTypeSymbol = "*"
ascii UnitTypeSymbol = "Unit"
ascii UnitSymbol = "unit"
ascii SumTypeSymbol = "+"
ascii InlSymbol = "inl"
ascii InrSymbol = "inr"
ascii ManyQuantitySymbol = "_"
ascii ColonSymbol = ":"
ascii DoubleColonSymbol = "::"

color :: SpecialSymbol -> Term.Color
color UniverseTypeSymbol = Term.Black
color PiTypeSymbol = Term.Black
color LambdaSymbol = Term.Black
color ArrowSymbol = Term.Black
color TensorTypeSymbol = Term.Black
color UnitTypeSymbol = Term.Black
color UnitSymbol = Term.Black
color SumTypeSymbol = Term.Black
color InlSymbol = Term.Black
color InrSymbol = Term.Black
color ManyQuantitySymbol = Term.Black
color ColonSymbol = Term.Black
color DoubleColonSymbol = Term.Black

format :: Bool -> SpecialSymbol -> Doc
format True = PP.pretty . unicode
format False = PP.pretty . ascii

annotateSpecialSymbol :: Bool -> SpecialSymbol -> Doc
annotateSpecialSymbol b s = annotate (Term.color (color s)) (format b s)

--------------------------------------------------------------------------------

class Pretty a where
  pretty :: Bool -> a -> Doc

printList :: [a] -> IO ()
printList = undefined
