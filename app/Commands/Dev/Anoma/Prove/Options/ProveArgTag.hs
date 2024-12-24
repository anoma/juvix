{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.Anoma.Prove.Options.ProveArgTag where

import CommonOptions
import Prelude qualified

data ProveArgTag
  = ProveArgTagNat
  | ProveArgTagBase64
  | ProveArgTagBytes
  deriving stock (Eq, Bounded, Enum, Data)

instance Show ProveArgTag where
  show = \case
    ProveArgTagNat -> "nat"
    ProveArgTagBase64 -> "base64"
    ProveArgTagBytes -> "bytes"

type ProveArgType :: ProveArgTag -> GHCType
type family ProveArgType s = res where
  ProveArgType 'ProveArgTagNat = Natural
  ProveArgType 'ProveArgTagBase64 = AppPath File
  ProveArgType 'ProveArgTagBytes = AppPath File

$(genDefunSymbols [''ProveArgType])
$(genSingletons [''ProveArgTag])

proveArgTagHelp :: AnsiDoc
proveArgTagHelp = itemize (tagHelp <$> allElements)
  where
    tagHelp :: ProveArgTag -> AnsiDoc
    tagHelp t =
      let mvar, explain :: AnsiDoc
          (mvar, explain) = first sty $ case t of
            ProveArgTagNat -> ("NATURAL", "is passed verbatim as a nockma atom")
            ProveArgTagBase64 -> ("FILE", "is a file with a base64 encoded nockma atom")
            ProveArgTagBytes -> ("FILE", "is a file with a byte encoded nockma atom")
          sty = annotate (bold <> colorDull Blue)
          tagvar :: AnsiDoc
          tagvar = sty (show t <> ":" <> mvar)
       in tagvar <+> "where" <+> mvar <+> explain
