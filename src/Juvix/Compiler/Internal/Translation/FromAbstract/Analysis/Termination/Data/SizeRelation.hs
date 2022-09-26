module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Data.SizeRelation where

import Juvix.Prelude
import Prettyprinter

data Rel
  = RJust Rel'
  | RNothing
  deriving stock (Eq, Show, Generic)

data Rel'
  = REq
  | RLe
  deriving stock (Eq, Show, Generic)

instance Hashable Rel'

instance Hashable Rel

toRel :: Rel' -> Rel
toRel = RJust

mul' :: Rel' -> Rel' -> Rel'
mul' REq a = a
mul' RLe _ = RLe

instance Pretty Rel where
  pretty r = case r of
    RJust r' -> pretty r'
    RNothing -> pretty ("?" :: Text)

instance Pretty Rel' where
  pretty r = case r of
    REq -> pretty ("=" :: Text)
    RLe -> pretty ("≺" :: Text)
