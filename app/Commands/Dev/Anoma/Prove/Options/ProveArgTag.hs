{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.Anoma.Prove.Options.ProveArgTag where

import CommonOptions
import Prelude qualified

data ProveArgTag
  = ProveArgTagNat
  | ProveArgTagBase64
  | ProveArgTagBytes
  | ProveArgTagBytesUnJammed
  | ProveArgTagBase64UnJammed
  deriving stock (Eq, Bounded, Enum, Data)

instance Show ProveArgTag where
  show = \case
    ProveArgTagNat -> "nat"
    ProveArgTagBase64 -> "base64"
    ProveArgTagBytes -> "bytes"
    ProveArgTagBytesUnJammed -> "bytes-unjammed"
    ProveArgTagBase64UnJammed -> "base64-unjammed"

type ProveArgType :: ProveArgTag -> GHCType
type family ProveArgType s = res where
  ProveArgType 'ProveArgTagNat = Natural
  ProveArgType 'ProveArgTagBase64 = AppPath File
  ProveArgType 'ProveArgTagBytes = AppPath File
  ProveArgType 'ProveArgTagBytesUnJammed = AppPath File
  ProveArgType 'ProveArgTagBase64UnJammed = AppPath File

$(genDefunSymbols [''ProveArgType])
$(genSingletons [''ProveArgTag])

proveArgTagHelp :: AnsiDoc
proveArgTagHelp = itemize (tagHelp <$> allElements)
  where
    tagHelp :: ProveArgTag -> AnsiDoc
    tagHelp t =
      let mvar, explain :: AnsiDoc
          jammedNoun :: AnsiDoc = annotate bold "jammed noun"
          unjammedAtom :: AnsiDoc = annotate bold "unjammed atom"
          (mvar, explain) = first sty $ case t of
            ProveArgTagNat -> ("NATURAL", "is passed verbatim as a nockma atom")
            ProveArgTagBase64 -> ("FILE", "is a file containing a base64 encoded nockma atom that represents a" <+> jammedNoun)
            ProveArgTagBytes -> ("FILE", "is a file containing bytes of a nockma atom that represents a" <+> jammedNoun)
            ProveArgTagBase64UnJammed -> ("FILE", "is a file containing a base64 encoded nockma atom that represents an" <+> unjammedAtom)
            ProveArgTagBytesUnJammed -> ("FILE", "is a file containing bytes of a nockma atom that represents an" <+> unjammedAtom)
          sty = annotate (bold <> colorDull Blue)
          tagvar :: AnsiDoc
          tagvar = sty (show t <> ":" <> mvar)
       in tagvar <+> "where" <+> mvar <+> explain
