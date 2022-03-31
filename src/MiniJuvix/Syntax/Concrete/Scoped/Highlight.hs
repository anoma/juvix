module MiniJuvix.Syntax.Concrete.Scoped.Highlight where

import MiniJuvix.Syntax.Concrete.Scoped.Name
import MiniJuvix.Syntax.Concrete.Loc
import qualified MiniJuvix.Internal.Strings as Str
import MiniJuvix.Prelude
import Prettyprinter
import Prettyprinter.Render.Text

data Face =
  FaceConstructor
  | FaceInductive
  | FaceFunction
  | FaceModule
  | FaceAxiom

data Property =
  PropertyFace Face

data Instruction =
  SetProperty {
  _setPropertyInterval :: Interval,
  _setPropertyProperty :: Property
  }

data SExp =
  Symbol Text
  | List [SExp]
  | Quote SExp
  | Int Word64

makeLenses ''Instruction

go :: [Name] -> Text
go = renderSExp . progn . mapMaybe goName

progn :: [SExp] -> SExp
progn l = List (Symbol "progn" : l)

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
  List [Symbol "add-text-properties", start , end, face]
  where
  pos l = Int (succ (l ^. locOffset . unPos))
  start = pos (i ^. intStart)
  end = pos (i ^. intEnd)
  face = Quote (List [Symbol "face", faceSymbol ])
  faceSymbol = Symbol ("minijuvix-highlight-" <> faceSymbolStr <> "-face")
  faceSymbolStr = case f of
    FaceAxiom -> Str.axiom
    FaceInductive -> Str.inductive
    FaceConstructor -> Str.constructor
    FaceModule -> Str.module_
    FaceFunction -> Str.function

goName :: Name -> Maybe SExp
goName n = do
  f <- nameKindFace (n ^. nameKind)
  return (instr (getLoc n) f)

renderSExp :: SExp -> Text
renderSExp =
  renderStrict
  . layoutPretty defaultLayoutOptions
  . pretty

instance Pretty SExp where
  pretty = \case
    Symbol s -> pretty s
    Int s -> pretty s
    List l -> parens (sep (map pretty l))
    Quote l -> pretty '`' <> pretty l
