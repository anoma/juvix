{-# OPTIONS_GHC -Wno-orphans -Wno-unused-matches #-}

module MiniJuvix.Pretty
  ( module MiniJuvix.Utils.Pretty,
  )
where

--------------------------------------------------------------------------------

import MiniJuvix.Syntax.Core
import MiniJuvix.Syntax.Eval
import MiniJuvix.Typing.Utils
import MiniJuvix.Utils.Prelude
import MiniJuvix.Utils.Pretty
  ( Doc,
    Pretty (..),
    ascii,
    color,
    hardlines,
    printList,
    render,
    unicode,
  )

-- import qualified MiniJuvix.Utils.Pretty as PP
-- import qualified Prettyprinter.Render.Terminal as Term

--------------------------------------------------------------------------------

instance Pretty Text where
  pretty = undefined -- const PP.pretty

instance Pretty Int where
  pretty = undefined -- const PP.pretty

instance Pretty Quantity where
  pretty _ Zero = undefined
  pretty _ One = undefined
  pretty b Many = undefined

instance Pretty Relevance where
  pretty _ Relevant = undefined -- PP.pretty "!"
  pretty _ Irrelevant = undefined -- PP.pretty "-"

instance Pretty Name where
  pretty _ (Global n) = undefined -- PP.pretty n
  pretty _ (Local n _) = undefined -- PP.pretty n

instance Pretty Variable where
  pretty _ (Bound idx) = undefined -- PP.pretty idx
  pretty b (Free name) = undefined -- pretty b name

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

instance Pretty Term where
  pretty b (Checkable t) = pretty b t
  pretty b (Inferable t) = pretty b t

instance Pretty Value where
  pretty _ = undefined

instance Pretty Neutral where
  pretty _ = undefined

instance Pretty Binding where
  pretty _ = undefined

instance Pretty TypingContext where
  pretty _ = undefined
