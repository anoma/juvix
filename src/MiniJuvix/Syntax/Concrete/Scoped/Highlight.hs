module MiniJuvix.Syntax.Concrete.Scoped.Highlight where

import MiniJuvix.Internal.Strings qualified as Str
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Loc
import MiniJuvix.Syntax.Concrete.Parser.ParsedItem
import MiniJuvix.Syntax.Concrete.Scoped.Name
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

newtype Property
  = PropertyFace Face

data Instruction = SetProperty
  { _setPropertyInterval :: Interval,
    _setPropertyProperty :: Property
  }

data SExp
  = Symbol Text
  | App [SExp]
  | Pair SExp SExp
  | Quote SExp
  | Backquote SExp
  | Int Word64
  | String String

makeLenses ''Instruction

go :: [ParsedItem] -> [Name] -> Text
go items names =
  renderSExp
    ( progn
        ( map goParsedItem items
            <> mapMaybe colorName names
            <> map gotoDefName names
        )
    )

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
--  '(face minijuvix-highlight-constructor-face))
instr :: Interval -> Face -> SExp
instr i f =
  App [Symbol "add-text-properties", start, end, face]
  where
    pos l = Int (succ (l ^. locOffset . unPos))
    start = pos (i ^. intStart)
    end = pos (i ^. intEnd)
    face = Quote (App [Symbol "face", faceSymbol faceSymbolStr])
    faceSymbolStr = case f of
      FaceAxiom -> Str.axiom
      FaceInductive -> Str.inductive
      FaceConstructor -> Str.constructor
      FaceModule -> Str.module_
      FaceKeyword -> Str.keyword
      FaceFunction -> Str.function
      FaceNumber -> Str.number
      FaceString -> Str.string

faceSymbol :: Text -> SExp
faceSymbol faceSymbolStr = Symbol ("minijuvix-highlight-" <> faceSymbolStr <> "-face")

goParsedItem :: ParsedItem -> SExp
goParsedItem i = instr (getLoc i) face
  where
    face = case i ^. parsedTag of
      ParsedTagKeyword -> FaceKeyword
      ParsedTagLiteralInt -> FaceNumber
      ParsedTagLiteralString -> FaceString

colorName :: Name -> Maybe SExp
colorName n = do
  f <- nameKindFace (n ^. nameKind)
  return (instr (getLoc n) f)

gotoDefName :: Name -> SExp
gotoDefName n =
  App [Symbol "add-text-properties", start, end, goto]
  where
    i = getLoc n
    targetPos = succ (n ^. nameDefined . intStart . locOffset . unPos)
    targetFile = n ^. nameDefined . intFile
    goto = Quote (App [Symbol "minijuvix-goto", gotoPair])
    pos l = Int (succ (l ^. locOffset . unPos))
    start = pos (i ^. intStart)
    end = pos (i ^. intEnd)
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
