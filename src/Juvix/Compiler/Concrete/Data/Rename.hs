module Juvix.Compiler.Concrete.Data.Rename where

import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Concrete.Translation.FromSource.Lexer qualified as L
import Juvix.Compiler.Concrete.Translation.FromSource.ParserResultBuilder (ParserResultBuilder, ignoreParserResultBuilder)
import Juvix.Data.Keyword.All qualified as Kw
import Juvix.Data.TopModulePathKey
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude.Base
import Juvix.Prelude.Parsing
import Juvix.Prelude.Pretty

data RenameCondition = RenameCondition
  { _renamePackageName :: Maybe Text,
    _renameModulePath :: Maybe TopModulePathKey,
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

applyRename :: Name' n -> Rename -> Maybe Text
applyRename n r
  | checkRenameCondition (r ^. renameCondition) n = Just (r ^. renameNewName)
  | otherwise = Nothing

checkRenameCondition :: RenameCondition -> Name' n -> Bool
checkRenameCondition RenameCondition {..} n =
  checkEq _renameModulePath (modId ^. moduleIdPath)
    && checkEq _renamePackageName (modId ^. moduleIdPackage)
    && checkEq (Just _renameOldName) (n ^. nameVerbatim)
  where
    modId :: ModuleId
    modId = n ^. nameId . nameIdModuleId

    checkEq :: (Eq a) => Maybe a -> a -> Bool
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
      helper :: (Pretty str) => Maybe str -> Doc a
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

It should be that:
forall r :: Rename. parseRename (prettyText r) == r
--}

parseRename :: Text -> Either Text Rename
parseRename = parseHelperSem parser (run . ignoreParserResultBuilder)
  where
    parser :: forall r. (Members '[ParserResultBuilder] r) => ParsecS r Rename
    parser = do
      L.whiteSpace
      _renamePackageName <- optionalHelper L.identifier
      L.kw Kw.delimSemicolon
      _renameModulePath <- optionalHelper pmodulePath
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
        pmodulePath :: ParsecS r TopModulePathKey
        pmodulePath =
          nonEmptyToTopModulePathKey
            . fmap fst
            <$> L.dottedIdentifier

        optionalHelper :: ParsecS r a -> ParsecS r (Maybe a)
        optionalHelper p =
          chunk Str.underscore $> Nothing
            <|> Just <$> p

tt :: Text -> IO ()
tt t = case parseRename t of
  Left err -> putStrLn err
  Right x -> putStrLn (prettyText x)
