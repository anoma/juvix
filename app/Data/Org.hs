module Data.Org where

import Juvix.Prelude

data OrgFile = OrgFile
  { _orgMeta :: HashMap Text Text,
    _orgDoc :: OrgDoc
  }

data OrgDoc = OrgDoc
  { _orgBlocks :: [Block],
    _orgSections :: [Section]
  }

data Section = Section
  { _sectionHeading :: Text,
    _sectionProperties :: HashMap Text Text,
    _sectionDoc :: OrgDoc
  }

data CodeBlock = CodeBlock {
  _codeLanguage :: Maybe Text,
  -- | The interval corresponds to the actual code. I.e. it does not include de
  -- #begin or #end
  _codeLoc :: Interval,
  _codeCode :: Text
  }

data Block =
  BlockCode CodeBlock
  | BlockParagraph Text
