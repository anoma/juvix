module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Data.SizeRelation where

import Juvix.Prelude
import Prettyprinter

data SizeRel
  = RJust SizeRel'
  | RNothing
  deriving stock (Eq, Show, Generic)

data SizeRel'
  = REq
  | RLe
  deriving stock (Eq, Show, Generic)

instance Hashable SizeRel'

instance Hashable SizeRel

toSizeRel :: SizeRel' -> SizeRel
toSizeRel = RJust

mul' :: SizeRel' -> SizeRel' -> SizeRel'
mul' REq a = a
mul' RLe _ = RLe

instance Pretty SizeRel where
  pretty r = case r of
    RJust r' -> pretty r'
    RNothing -> pretty ("?" :: Text)

instance Pretty SizeRel' where
  pretty r = case r of
    REq -> pretty ("=" :: Text)
    RLe -> pretty ("≺" :: Text)
