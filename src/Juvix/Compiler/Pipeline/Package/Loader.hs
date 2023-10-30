module Juvix.Compiler.Pipeline.Package.Loader where

import Data.FileEmbed qualified as FE
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Versions
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource
import Juvix.Compiler.Concrete.Translation.FromSource.Lexer
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Language.Value
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Extra.Paths
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Language.Haskell.TH.Syntax hiding (Module)
import System.FilePath qualified as FP

data PackageDescriptionType = PackageDescriptionType
  { _packageDescriptionTypePath :: Path Rel File,
    _packageDescriptionTypeName :: Text,
    _packageDescriptionTypeTransform :: Package -> FunctionDefBody 'Parsed,
    _packageDescriptionTypeNeedsStdlibImport :: Package -> Bool
  }

makeLenses ''PackageDescriptionType

v1PackageDescriptionType :: PackageDescriptionType
v1PackageDescriptionType = PackageDescriptionType v1PackageDescriptionFile "Package" fromPackage needsStdlib
  where
    needsStdlib :: Package -> Bool
    needsStdlib p =
      let SemVer {..} = p ^. packageVersion
       in isJust _svMeta || isJust _svPreRel

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

        mkNamedArgs :: NonEmpty (NamedArgument 'Parsed)
        mkNamedArgs = mkNameArg :| [mkVersionArg]
          where
            mkNameArg :: NamedArgument 'Parsed
            mkNameArg = mkNamedArg "name" ((mkLitText (p ^. packageName)) :| [])

            mkLitText :: Text -> ExpressionAtom 'Parsed
            mkLitText = AtomLiteral . WithLoc l . LitString

            mkIdentifier :: Text -> ExpressionAtom 'Parsed
            mkIdentifier = AtomIdentifier . NameUnqualified . mkSymbol

            mkJustArg :: ExpressionAtom 'Parsed -> ExpressionAtom 'Parsed
            mkJustArg a =
              AtomBraces
                ( WithLoc
                    l
                    ( ExpressionAtoms
                        { _expressionAtomsLoc = Irrelevant l,
                          _expressionAtoms = mkIdentifier "just" :| [a]
                        }
                    )
                )

            mkNothingArg :: ExpressionAtom 'Parsed
            mkNothingArg = AtomBraces (WithLoc l (ExpressionAtoms {_expressionAtomsLoc = Irrelevant l, _expressionAtoms = mkIdentifier "nothing" :| []}))

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
                { _namedArgValue =
                    ExpressionAtoms
                      { _expressionAtomsLoc = Irrelevant l,
                        _expressionAtoms = v
                      },
                  _namedArgName = mkSymbol n,
                  _namedArgAssignKw = Irrelevant (mkKeywordRef kwAssign)
                }

-- | The names of the Package type name in every version of the PackageDescription module
packageDescriptionTypes :: [PackageDescriptionType]
packageDescriptionTypes = [v1PackageDescriptionType]

acceptableTypes :: forall r. (Member Files r) => Sem r [TypeSpec]
acceptableTypes = mapM go packageDescriptionTypes
  where
    go :: PackageDescriptionType -> Sem r TypeSpec
    go t = do
      globalPackageDir <- globalPackageDescriptionRoot
      return
        TypeSpec
          { _typeSpecName = t ^. packageDescriptionTypeName,
            _typeSpecFile = globalPackageDir <//> (t ^. packageDescriptionTypePath)
          }

-- | Load a package file in the context of the PackageDescription module and the global package stdlib.
loadPackage :: (Members '[Files, EvalFileEff, Error PackageLoaderError] r) => BuildDir -> Path Abs File -> Sem r Package
loadPackage buildDir packagePath = do
  scoped @(Path Abs File) @EvalEff packagePath $ do
    v <- getPackageNode >>= eval'
    toPackage buildDir packagePath v
  where
    -- Obtain the Node corresponding to the `package` identifier in the loaded
    -- Package
    --
    -- This function also checks that the type of the identifier is among the
    -- expected types from the specific PackageDescription modules that are
    -- provided by the PathResolver.
    getPackageNode :: forall r. (Members '[Files, EvalEff] r) => Sem r Core.Node
    getPackageNode = do
      n <- lookupIdentifier Str.package
      acceptableTypes >>= assertNodeType n
      return n

toPackage ::
  forall r.
  (Member (Error PackageLoaderError) r) =>
  BuildDir ->
  Path Abs File ->
  Value ->
  Sem r Package
toPackage buildDir packagePath = \case
  ValueConstrApp ctor -> do
    case ctor ^. constrAppArgs of
      [vName, vVersion, vDeps, vMain, vBuildDir] -> do
        _packageName <- toText vName
        _packageMain <- toMaybeMain vMain
        _packageBuildDir <- toMaybeBuildDir vBuildDir
        _packageDependencies <- toList' toDependency vDeps
        _packageVersion <- toVersion vVersion
        return Package {_packageLockfile = Nothing, _packageFile = packagePath, ..}
      _ -> err
  _ -> err
  where
    err :: Sem r a
    err =
      throw
        PackageLoaderError
          { _packageLoaderErrorPath = packagePath,
            _packageLoaderErrorCause = ErrPackageTypeError
          }

    toMaybe :: (Value -> Sem r a) -> Value -> Sem r (Maybe a)
    toMaybe f = \case
      ValueConstrApp c -> case c ^. constrAppArgs of
        [] -> return Nothing
        [v] -> Just <$> f v
        _ -> err
      _ -> err

    toList' :: (Value -> Sem r a) -> Value -> Sem r [a]
    toList' f = \case
      ValueConstrApp c -> case c ^. constrAppArgs of
        [] -> return []
        [x, xs] -> do
          v <- f x
          vs <- toList' f xs
          return (v : vs)
        _ -> err
      _ -> err

    toText :: Value -> Sem r Text
    toText = \case
      ValueConstant (Core.ConstString s) -> return s
      _ -> err

    toInteger' :: Value -> Sem r Integer
    toInteger' = \case
      ValueConstant (Core.ConstInteger i) -> return i
      _ -> err

    toWord :: Value -> Sem r Word
    toWord = fmap fromInteger . toInteger'

    toMaybeMain :: Value -> Sem r (Maybe (Prepath File))
    toMaybeMain = toMaybe (fmap (mkPrepath . unpack) . toText)

    toMaybeBuildDir :: Value -> Sem r (Maybe (SomeBase Dir))
    toMaybeBuildDir = toMaybe go
      where
        go :: Value -> Sem r (SomeBase Dir)
        go v = do
          s <- unpack <$> toText v
          let p :: Maybe (SomeBase Dir)
              p = (Abs <$> parseAbsDir s) <|> (Rel <$> parseRelDir s)
          maybe err return p

    toVersion :: Value -> Sem r SemVer
    toVersion = \case
      ValueConstrApp c -> case c ^. constrAppArgs of
        [vMaj, vMin, vPatch, _, vMeta] -> do
          maj <- toWord vMaj
          min' <- toWord vMin
          patch' <- toWord vPatch
          meta' <- toMaybe toText vMeta
          return (SemVer maj min' patch' Nothing meta')
        _ -> err
      _ -> err

    toDependency :: Value -> Sem r Dependency
    toDependency = \case
      ValueConstrApp c -> case c ^. constrAppArgs of
        [] -> return defaultStdlib
        [v] -> do
          p <- mkPrepath . unpack <$> toText v
          return (DependencyPath (PathDependency {_pathDependencyPath = p}))
        [vName, vUrl, vRef] -> do
          _gitDependencyUrl <- toText vUrl
          _gitDependencyName <- toText vName
          _gitDependencyRef <- toText vRef
          return (DependencyGit (GitDependency {..}))
        _ -> err
      _ -> err

    defaultStdlib :: Dependency
    defaultStdlib = mkPathDependency (fromSomeDir p)
      where
        p :: SomeBase Dir
        p = resolveBuildDir buildDir <///> relStdlibDir

toConcrete :: PackageDescriptionType -> Package -> Module 'Parsed 'ModuleTop
toConcrete t p = do
  let _importModule :: TopModulePath = mkTopModulePath (fromJust (nonEmpty (mkSymbol . pack . FP.dropExtension <$> FP.splitDirectories (toFilePath (t ^. packageDescriptionTypePath)))))
      retTy :: ExpressionAtoms 'Parsed =
        ExpressionAtoms
          { _expressionAtomsLoc = Irrelevant l,
            _expressionAtoms = AtomIdentifier (NameUnqualified (mkSymbol (t ^. packageDescriptionTypeName))) :| []
          }
      stdlibImport :: [Statement 'Parsed]
        | (t ^. packageDescriptionTypeNeedsStdlibImport) p = [mkStdlibImport]
        | otherwise = []
      _moduleBody :: [Statement 'Parsed] =
        stdlibImport
          <> [ mkImport _importModule,
               StatementFunctionDef
                 FunctionDef
                   { _signTerminating = Nothing,
                     _signRetType = Just retTy,
                     _signPragmas = Nothing,
                     _signName = mkSymbol Str.package,
                     _signInstance = Nothing,
                     _signDoc = Nothing,
                     _signColonKw = Irrelevant (Just (mkKeywordRef kwColon)),
                     _signCoercion = Nothing,
                     _signBuiltin = Nothing,
                     _signBody = (t ^. packageDescriptionTypeTransform) p,
                     _signArgs = []
                   }
             ]
   in Module
        { _modulePath = mkTopModulePath (mkSymbol "Package" :| []),
          _moduleKwEnd = (),
          _moduleInductive = (),
          _moduleDoc = Nothing,
          _modulePragmas = Nothing,
          _moduleKw = mkKeywordRef kwModule,
          ..
        }
  where
    mkImport :: TopModulePath -> Statement 'Parsed
    mkImport _importModule =
      StatementImport
        Import
          { _importOpen =
              Just
                OpenModuleParams
                  { _openUsingHiding = Nothing,
                    _openPublicKw = Irrelevant Nothing,
                    _openPublic = NoPublic,
                    _openModuleKw = mkKeywordRef kwOpen
                  },
            _importKw = mkKeywordRef kwImport,
            _importAsName = Nothing,
            ..
          }

    mkStdlibImport :: Statement 'Parsed
    mkStdlibImport = mkImport (mkTopModulePath (mkSymbol "Stdlib" :| [mkSymbol "Prelude"]))

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

packageDescriptionDir' :: Path Abs Dir
packageDescriptionDir' =
  $( FE.makeRelativeToProject (toFilePath packageDescriptionDir)
       >>= runIO
         . parseAbsDir
       >>= lift
   )
