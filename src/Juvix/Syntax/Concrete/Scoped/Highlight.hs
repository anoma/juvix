module Juvix.Syntax.Concrete.Scoped.Highlight where

import Juvix.Internal.Strings qualified as Str
import Juvix.Prelude
import Juvix.Syntax.Concrete.Parser.ParsedItem
import Juvix.Syntax.Concrete.Scoped.Name
import Prettyprinter
import Prettyprinter.Render.Text

data Face
  = FaceConstructor
  | FaceInductive
  | FaceFunction
  | FaceModule
  | FaceAxiom
  | FaceKeyword
  | FaceString
  | FaceNumber
  | FaceComment
  | FaceError

newtype Property
  = PropertyFace Face

data Instruction = SetProperty
  { _setPropertyInterval :: Interval,
    _setPropertyProperty :: Property
  }

data HighlightInput = HighlightInput
  { _highlightParsed :: [ParsedItem],
    _highlightNames :: [AName]
  }

makeLenses ''HighlightInput

data SExp
  = Symbol Text
  | App [SExp]
  | Pair SExp SExp
  | Quote SExp
  | Backquote SExp
  | Int Word64
  | String String

makeLenses ''Instruction

filterInput :: FilePath -> HighlightInput -> HighlightInput
filterInput absPth HighlightInput {..} =
  HighlightInput
    { _highlightNames = filterByLoc absPth _highlightNames,
      _highlightParsed = filterByLoc absPth _highlightParsed
    }

filterByLoc :: HasLoc p => FilePath -> [p] -> [p]
filterByLoc absPth = filter ((== absPth) . (^. intervalFile) . getLoc)

goError :: [Interval] -> Text
goError l =
  renderSExp
    (progn (map goIntervalErr l))
  where
    goIntervalErr :: Interval -> SExp
    goIntervalErr = instr FaceError

go :: HighlightInput -> Text
go HighlightInput {..} =
  renderSExp
    ( progn
        ( map goParsedItem items
            <> mapMaybe colorName names
            <> map gotoDefName names
        )
    )
  where
    names = _highlightNames
    items :: [ParsedItem]
    items = _highlightParsed

progn :: [SExp] -> SExp
progn l = App (Symbol "progn" : l)

nameKindFace :: NameKind -> Maybe Face
nameKindFace = \case
  KNameConstructor -> Just FaceConstructor
  KNameInductive -> Just FaceInductive
  KNameFunction -> Just FaceFunction
  KNameTopModule -> Just FaceModule
  KNameLocalModule -> Just FaceModule
  KNameAxiom -> Just FaceAxiom
  KNameLocal -> Nothing

-- | Example instruction:
-- (add-text-properties 20 28
--  '(face juvix-highlight-constructor-face))
instr :: Face -> Interval -> SExp
instr f i =
  App [Symbol "add-text-properties", start, end, face]
  where
    pos l = Int (succ (l ^. locOffset . unPos))
    start = pos (i ^. intervalStart)
    end = pos (i ^. intervalEnd)
    face = Quote (App [Symbol "face", faceSymbol faceSymbolStr])
    faceSymbolStr = case f of
      FaceAxiom -> Str.axiom
      FaceInductive -> Str.inductive
      FaceConstructor -> Str.constructor
      FaceModule -> Str.module_
      FaceKeyword -> Str.keyword
      FaceFunction -> Str.function
      FaceNumber -> Str.number
      FaceComment -> Str.comment
      FaceString -> Str.string
      FaceError -> Str.error

faceSymbol :: Text -> SExp
faceSymbol faceSymbolStr = Symbol ("juvix-highlight-" <> faceSymbolStr <> "-face")

goParsedItem :: ParsedItem -> SExp
goParsedItem i = instr face (getLoc i)
  where
    face = case i ^. parsedTag of
      ParsedTagKeyword -> FaceKeyword
      ParsedTagLiteralInt -> FaceNumber
      ParsedTagLiteralString -> FaceString
      ParsedTagComment -> FaceComment

colorName :: AName -> Maybe SExp
colorName n = do
  f <- nameKindFace (getNameKind n)
  return (instr f (getLoc n))

gotoDefName :: AName -> SExp
gotoDefName (AName n) =
  App [Symbol "add-text-properties", start, end, goto]
  where
    i = getLoc n
    targetPos = succ (n ^. nameDefined . intervalStart . locOffset . unPos)
    targetFile = n ^. nameDefined . intervalFile
    goto = Quote (App [Symbol "juvix-goto", gotoPair])
    pos l = Int (succ (l ^. locOffset . unPos))
    start = pos (i ^. intervalStart)
    end = pos (i ^. intervalEnd)
    gotoPair = Pair (String targetFile) (Int targetPos)

renderSExp :: SExp -> Text
renderSExp =
  renderStrict
    . layoutPretty defaultLayoutOptions
    . pretty

instance Pretty SExp where
  pretty = \case
    Symbol s -> pretty s
    Int s -> pretty s
    App l -> parens (sep (map pretty l))
    Pair l r -> parens (pretty l <+> dot <+> pretty r)
    Backquote l -> pretty '`' <> pretty l
    Quote l -> pretty '\'' <> pretty l
    String s -> dquotes (pretty s)
