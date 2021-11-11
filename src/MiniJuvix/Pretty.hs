module MiniJuvix.Print 
  (module MiniJuvix.Utils.Pretty
  )
  where

--------------------------------------------------------------------------------

import MiniJuvix.Utils.Pretty

--------------------------------------------------------------------------------

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