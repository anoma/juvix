module Juvix.Compiler.Concrete.Data.Rename where

import Data.Text qualified as Text
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Concrete.Translation.FromSource.Lexer qualified as L
import Juvix.Compiler.Concrete.Translation.FromSource.ParserResultBuilder (ParserResultBuilder, ignoreParserResultBuilder)
import Juvix.Data.Keyword.All qualified as Kw
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude.Base
import Juvix.Prelude.Parsing
import Juvix.Prelude.Pretty

data RenameCondition = RenameCondition
  { _renamePackageName :: Maybe Text,
    _renameModulePath :: Maybe Text,
    _renameOldName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

data Rename = Rename
  { _renameCondition :: RenameCondition,
    _renameNewName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

makeLenses ''Rename
makeLenses ''RenameCondition

instance Hashable RenameCondition

instance Hashable Rename

checkRenameCondition :: RenameCondition -> Name' n -> Bool
checkRenameCondition RenameCondition {..} n =
  checkEq _renameModulePath (modId ^. moduleIdPath)
    && checkEq _renamePackageName (modId ^. moduleIdPackage)
    && checkEq _renamePackageName (n ^. nameVerbatim)
  where
    modId :: ModuleId
    modId = n ^. nameId . nameIdModuleId
    checkEq :: Maybe Text -> Text -> Bool
    checkEq mt1 t2 = case mt1 of
      Nothing -> True
      Just t1 -> t1 == t2

instance Pretty RenameCondition where
  pretty RenameCondition {..} =
    helper _renamePackageName
      <> pretty Kw.delimSemicolon
      <> helper _renameModulePath
      <> pretty Kw.delimSemicolon
      <> pretty _renameOldName
    where
      helper :: Maybe Text -> Doc a
      helper = maybe Str.underscore pretty

instance Pretty Rename where
  pretty Rename {..} =
    pretty _renameCondition
      <> " "
      <> pretty Kw.kwMapsTo
      <> " "
      <> pretty _renameNewName

{--| Rename syntax:
<packageName>;<modulePath>;<oldname> -> <newname>

Examples:
stdlib;Stdlib.Function;if -> ite
_;Stdlib.Function;if -> ite

note that the space before '->' is mandatory because - is not a reserved symbol.
--}

parseRename :: Text -> Either Text Rename
parseRename = parseHelperSem parser (run . ignoreParserResultBuilder)
  where
    parser :: forall r. (Members '[ParserResultBuilder] r) => ParsecS r Rename
    parser = do
      L.whiteSpace
      _renamePackageName <- stringHelper L.identifier
      L.kw Kw.delimSemicolon
      _renameModulePath <- stringHelper pmodulePath
      L.kw Kw.delimSemicolon
      _renameOldName <- L.identifier
      L.kw Kw.kwMapsTo
      _renameNewName <- L.identifier
      eof
      let _renameCondition =
            RenameCondition
              { _renamePackageName,
                _renameModulePath,
                _renameOldName
              }
      return
        Rename
          { _renameCondition,
            _renameNewName
          }
      where
        pmodulePath :: ParsecS r Text
        pmodulePath =
          Text.intercalate Str.dot
            . fmap fst
            . toList
            <$> L.dottedIdentifier

        stringHelper :: ParsecS r Text -> ParsecS r (Maybe Text)
        stringHelper p =
          chunk Str.underscore $> Nothing
            <|> Just <$> p

tt :: Text -> IO ()
tt t = case parseRename t of
  Left err -> putStrLn err
  Right x -> putStrLn (prettyText x)
