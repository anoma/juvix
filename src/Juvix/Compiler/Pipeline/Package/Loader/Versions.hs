module Juvix.Compiler.Pipeline.Package.Loader.Versions where

import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Versions
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.Lexer
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Extra.Paths
import Juvix.Prelude

data PackageDescriptionType = PackageDescriptionType
  { _packageDescriptionTypePath :: Path Rel File,
    _packageDescriptionTypeName :: Text,
    _packageDescriptionTypeTransform :: Package -> FunctionDefBody 'Parsed,
    _packageDescriptionTypeNeedsStdlibImport :: Package -> Bool
  }

makeLenses ''PackageDescriptionType

-- | The names of the Package type name in every version of the PackageDescription module
packageDescriptionTypes :: [PackageDescriptionType]
packageDescriptionTypes = [v1PackageDescriptionType]

v1PackageDescriptionType :: PackageDescriptionType
v1PackageDescriptionType = PackageDescriptionType v1PackageDescriptionFile "Package" fromPackage needsStdlib
  where
    needsStdlib :: Package -> Bool
    needsStdlib p =
      let SemVer {..} = p ^. packageVersion
       in isJust _svMeta || isJust _svPreRel

    -- Supports the subset of Package fields that are customizable with `juvix init`
    fromPackage :: Package -> FunctionDefBody 'Parsed
    fromPackage p =
      SigBodyExpression
        ExpressionAtoms
          { _expressionAtomsLoc = Irrelevant l,
            _expressionAtoms =
              AtomNamedApplication
                NamedApplication
                  { _namedAppName = NameUnqualified (mkSymbol "defaultPackage"),
                    _namedAppArgs =
                      ArgumentBlock
                        { _argBlockImplicit = Implicit,
                          _argBlockDelims = Irrelevant (Just (mkKeywordRef delimBraceL, mkKeywordRef delimBraceR)),
                          _argBlockArgs = mkNamedArgs
                        }
                        :| []
                  }
                :| []
          }
      where
        l :: Interval
        l = singletonInterval (mkInitialLoc (p ^. packageFile))

        mkSymbol :: Text -> Symbol
        mkSymbol = WithLoc l

        mkKeywordRef :: Keyword -> KeywordRef
        mkKeywordRef k =
          KeywordRef
            { _keywordRefUnicode = Ascii,
              _keywordRefKeyword = k,
              _keywordRefInterval = l
            }

        mkExpressionAtoms :: NonEmpty (ExpressionAtom 'Parsed) -> ExpressionAtoms 'Parsed
        mkExpressionAtoms as =
          ExpressionAtoms
            { _expressionAtomsLoc = Irrelevant l,
              _expressionAtoms = as
            }

        mkNamedArgs :: NonEmpty (NamedArgument 'Parsed)
        mkNamedArgs = mkNameArg :| mkVersionArg : mkDependenciesArg : catMaybes [mkMainArg, mkBuildDirArg]
          where
            mkNameArg :: NamedArgument 'Parsed
            mkNameArg = mkNamedArg "name" (mkLitText (p ^. packageName) :| [])

            mkDependenciesArg :: NamedArgument 'Parsed
            mkDependenciesArg = mkNamedArg "dependencies" (mkList (mkDependencyArg <$> p ^. packageDependencies))
              where
                mkDependencyArg :: Dependency -> NonEmpty (ExpressionAtom 'Parsed)
                mkDependencyArg = \case
                  DependencyPath x -> mkIdentifier "path" :| [mkLitText (pack (unsafePrepathToFilePath (x ^. pathDependencyPath)))]
                  DependencyGit x -> mkIdentifier "git" :| [mkLitText (x ^. gitDependencyName), mkLitText (x ^. gitDependencyUrl), mkLitText (x ^. gitDependencyRef)]

                mkList :: [NonEmpty (ExpressionAtom 'Parsed)] -> NonEmpty (ExpressionAtom 'Parsed)
                mkList as =
                  AtomList
                    List
                      { _listItems = mkExpressionAtoms <$> as,
                        _listBracketR = Irrelevant (mkKeywordRef kwBracketR),
                        _listBracketL = Irrelevant (mkKeywordRef kwBracketL)
                      }
                    :| []

            mkMainArg :: Maybe (NamedArgument 'Parsed)
            mkMainArg = mkNamedArg "main" . mainArg <$> p ^. packageMain
              where
                mainArg :: Prepath File -> NonEmpty (ExpressionAtom 'Parsed)
                mainArg = mkJust . mkLitText . pack . unsafePrepathToFilePath

            mkBuildDirArg :: Maybe (NamedArgument 'Parsed)
            mkBuildDirArg = mkNamedArg "buildDir" . buildDirArg <$> p ^. packageBuildDir
              where
                buildDirArg :: SomeBase Dir -> NonEmpty (ExpressionAtom 'Parsed)
                buildDirArg = mkJust . mkLitText . pack . fromSomeDir

            mkLitText :: Text -> ExpressionAtom 'Parsed
            mkLitText = AtomLiteral . WithLoc l . LitString

            mkIdentifier :: Text -> ExpressionAtom 'Parsed
            mkIdentifier = AtomIdentifier . NameUnqualified . mkSymbol

            mkJust :: ExpressionAtom 'Parsed -> NonEmpty (ExpressionAtom 'Parsed)
            mkJust a = mkIdentifier "just" :| [a]

            mkJustArg :: ExpressionAtom 'Parsed -> ExpressionAtom 'Parsed
            mkJustArg a =
              AtomBraces
                ( WithLoc
                    l
                    (mkExpressionAtoms (mkJust a))
                )

            mkNothingArg :: ExpressionAtom 'Parsed
            mkNothingArg =
              AtomBraces
                ( WithLoc
                    l
                    (mkExpressionAtoms (mkIdentifier "nothing" :| []))
                )

            mkVersionArg :: NamedArgument 'Parsed
            mkVersionArg = mkNamedArg "version" ((mkIdentifier "mkVersion") :| args)
              where
                args :: [ExpressionAtom 'Parsed]
                args = wordArgs <> optionalArgs

                optionalArgs :: [ExpressionAtom 'Parsed]
                optionalArgs = case (releaseArg, metaArg) of
                  (Nothing, Nothing) -> []
                  (Nothing, Just ma) -> mkNothingArg : [mkJustArg ma]
                  (Just ra, Nothing) -> [mkJustArg ra]
                  (Just ra, Just ma) -> mkJustArg ra : [mkJustArg ma]

                mkWordArg :: Word -> ExpressionAtom 'Parsed
                mkWordArg w = AtomLiteral (WithLoc l (LitInteger (toInteger w)))

                wordArgs :: [ExpressionAtom 'Parsed]
                wordArgs =
                  let SemVer {..} = p ^. packageVersion
                   in mkWordArg <$> [_svMajor, _svMinor, _svPatch]

                releaseArg :: Maybe (ExpressionAtom 'Parsed)
                releaseArg = let SemVer {..} = p ^. packageVersion in mkReleaseArg <$> _svPreRel
                  where
                    mkReleaseArg :: Release -> ExpressionAtom 'Parsed
                    mkReleaseArg = mkLitText . prettyRelease

                    prettyRelease :: Release -> Text
                    prettyRelease (Release cs) = T.intercalate "." . map prettyChunk $ NEL.toList cs

                    prettyChunk :: Chunk -> Text
                    prettyChunk (Numeric n) = show n
                    prettyChunk (Alphanum s) = s

                metaArg :: Maybe (ExpressionAtom 'Parsed)
                metaArg = let SemVer {..} = p ^. packageVersion in mkLitText <$> _svMeta

            mkNamedArg :: Text -> NonEmpty (ExpressionAtom 'Parsed) -> NamedArgument 'Parsed
            mkNamedArg n v =
              NamedArgument
                { _namedArgValue = mkExpressionAtoms v,
                  _namedArgName = mkSymbol n,
                  _namedArgAssignKw = Irrelevant (mkKeywordRef kwAssign)
                }
