module MiniJuvix.Utils.Pretty
  ( Doc,
    Pretty (..),
    unicode,
    ascii,
    color,
    render,
    hardlines,
  )
where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

import MiniJuvix.Syntax.Core
import MiniJuvix.Syntax.Eval
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

instance Pretty Text where
  pretty = const PP.pretty

instance Pretty Int where
  pretty = const PP.pretty

instance Pretty Quantity where
  pretty _ Zero = annotate (Term.color Term.Magenta) (PP.pretty "0")
  pretty _ One = annotate (Term.color Term.Magenta) (PP.pretty "1")
  pretty b Many = annotateSpecialSymbol b ManyQuantitySymbol

instance Pretty Relevance where
  pretty _ Relevant = PP.pretty "!"
  pretty _ Irrelevant = PP.pretty "-"

instance Pretty Name where
  pretty _ (Global n) = PP.pretty n
  pretty _ (Local n _) = PP.pretty n

instance Pretty Variable where
  pretty _ (Bound idx) = PP.pretty idx
  pretty b (Free name) = pretty b name

instance Pretty Term where
  pretty b (Checkable t) = pretty b t
  pretty b (Inferable t) = pretty b t

instance Pretty CheckableTerm where
  pretty _ = prettyCheckable

prettyCheckable :: CheckableTerm -> Doc
prettyCheckable UniverseType = undefined
prettyCheckable (PiType q x s t) = undefined
prettyCheckable (Lam x t) = undefined
prettyCheckable (TensorType q x s t) = undefined
prettyCheckable (TensorIntro s t) = undefined
prettyCheckable UnitType = undefined
prettyCheckable Unit = undefined
prettyCheckable (SumType s t) = undefined
prettyCheckable (Inl x) = undefined
prettyCheckable (Inr x) = undefined
prettyCheckable (Inferred x) = undefined

instance Pretty InferableTerm where
  pretty _ = prettyInferrable

prettyInferrable :: InferableTerm -> Doc
prettyInferrable (Var x) = undefined
prettyInferrable (Ann x t) = undefined
prettyInferrable (App m n) = undefined
prettyInferrable (TensorTypeElim q x _ _ t _ _) = undefined
prettyInferrable (SumTypeElim q b x y c z s t) = undefined

instance Pretty Value where
  pretty _ = undefined

instance Pretty Neutral where
  pretty _ = undefined
