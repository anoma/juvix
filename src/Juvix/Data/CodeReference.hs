module Juvix.Data.CodeReference where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Data.NameId
import Juvix.Data.NameKind
import Juvix.Prelude.Base

-- | `TopCodeAnnReference`s can be used in links because they are stable accross compiler versions and
-- reordering of definitions. Note that if the symbol is moved to a different local module the link will break.
data TopCodeReference = TopCodeReference
  { _topCodeReferenceAbsModule :: AbsModulePath,
    _topCodeReferenceVerbatimSymbol :: Text
  }

-- | `LocalCodeReference`s should not be used in links as they will most
-- likely break or point to the incorrect symbol if the order of definitions
-- change or the compiler is updated
data LocalCodeReference = LocalCodeReference
  { _localCodeReferenceModule :: TopModulePath,
    _localCodeReferenceNameId :: NameId
  }

data CodeReferenceLoc
  = CodeReferenceLocLocal LocalCodeReference
  | CodeReferenceLocTop TopCodeReference

data CodeReference = CodeReference
  { _codeReferenceLoc :: CodeReferenceLoc,
    _codeReferenceNameKindPretty :: NameKind
  }

makeLenses ''CodeReference
makeLenses ''TopCodeReference
makeLenses ''LocalCodeReference

instance HasNameKind CodeReference where
  getNameKind = (^. codeReferenceNameKindPretty)
  getNameKindPretty = (^. codeReferenceNameKindPretty)

codeReferenceLocTopModule :: Lens' CodeReferenceLoc TopModulePath
codeReferenceLocTopModule f = \case
  CodeReferenceLocLocal l -> CodeReferenceLocLocal <$> localCodeReferenceModule f l
  CodeReferenceLocTop l -> CodeReferenceLocTop <$> (topCodeReferenceAbsModule . absTopModulePath) f l
