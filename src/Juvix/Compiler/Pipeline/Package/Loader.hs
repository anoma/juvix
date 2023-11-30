module Juvix.Compiler.Pipeline.Package.Loader
  ( module Juvix.Compiler.Pipeline.Package.Loader,
    module Juvix.Compiler.Pipeline.Package.Loader.Versions,
  )
where

import Data.FileEmbed qualified as FE
import Juvix.Compiler.Concrete.Gen
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print (ppOutDefaultNoComments)
import Juvix.Compiler.Concrete.Translation.FromSource hiding (symbol)
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Package.Loader.Versions
import Juvix.Extra.Paths
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Language.Haskell.TH.Syntax hiding (Module)
import System.FilePath qualified as FP

acceptableTypes :: forall r. (Member Files r) => Sem r [TypeSpec]
acceptableTypes = mapM go packageDescriptionTypes
  where
    go :: PackageDescriptionType -> Sem r TypeSpec
    go t = do
      globalPackageDir <- globalPackageDescriptionRoot
      return
        TypeSpec
          { _typeSpecName = t ^. packageDescriptionTypeName,
            _typeSpecFile = globalPackageDir <//> (t ^. packageDescriptionTypePath),
            _typeSpecVersion = t ^. packageDescriptionTypeVersion
          }

renderPackageVersion :: PackageVersion -> Package -> Text
renderPackageVersion v pkg = toPlainText (ppOutDefaultNoComments (toConcrete (getPackageType v) pkg))

-- | Load a package file in the context of the PackageDescription module and the global package stdlib.
loadPackage :: (Members '[Files, EvalFileEff, Error PackageLoaderError] r) => BuildDir -> Path Abs File -> Sem r Package
loadPackage buildDir packagePath = do
  scoped @(Path Abs File) @EvalEff packagePath $ do
    (v, t) <- getPackageNode
    ((getPackageType (t ^. typeSpecVersion)) ^. packageDescriptionTypeToPackage) buildDir packagePath =<< eval' v
  where
    -- Obtain the Node corresponding to the `package` identifier in the loaded
    -- Package
    --
    -- This function also checks that the type of the identifier is among the
    -- expected types from the specific PackageDescription modules that are
    -- provided by the PathResolver.
    getPackageNode :: forall r. (Members '[Files, EvalEff] r) => Sem r (Core.Node, TypeSpec)
    getPackageNode = do
      n <- lookupIdentifier Str.package
      ty <- acceptableTypes >>= assertNodeType n
      return (n, ty)

toConcrete :: PackageDescriptionType -> Package -> Module 'Parsed 'ModuleTop
toConcrete t p = run . runReader l $ do
  packageSymbol <- symbol "Package"
  importModuleSymbols <- mapM symbol (pack . FP.dropExtension <$> FP.splitDirectories (toFilePath (t ^. packageDescriptionTypePath)))
  let _importModule :: TopModulePath = mkTopModulePath (fromJust (nonEmpty importModuleSymbols))
  _moduleBody :: [Statement 'Parsed] <- do
    stdlib <- maybeToList <$> stdlibImport
    body <- sequence [mkImport _importModule, funDef]
    return (stdlib <> body)
  _moduleKw <- kw kwModule
  let _modulePath = mkTopModulePath (packageSymbol :| [])
  return
    Module
      { _moduleKwEnd = (),
        _moduleInductive = (),
        _moduleDoc = Nothing,
        _modulePragmas = Nothing,
        _moduleMarkdownInfo = Nothing,
        ..
      }
  where
    funDef :: (Member (Reader Interval) r) => Sem r (Statement 'Parsed)
    funDef = do
      packageTypeIdentifier <- identifier (t ^. packageDescriptionTypeName)
      _signRetType <- Just <$> expressionAtoms' (packageTypeIdentifier :| [])
      _signName <- symbol Str.package
      _signColonKw <- Irrelevant . Just <$> kw kwColon
      let _signBody = (t ^. packageDescriptionTypeTransform) p
      return
        ( StatementFunctionDef
            FunctionDef
              { _signTerminating = Nothing,
                _signPragmas = Nothing,
                _signInstance = Nothing,
                _signDoc = Nothing,
                _signCoercion = Nothing,
                _signBuiltin = Nothing,
                _signArgs = [],
                ..
              }
        )

    stdlibImport :: (Member (Reader Interval) r) => Sem r (Maybe (Statement 'Parsed))
    stdlibImport
      | (t ^. packageDescriptionTypeNeedsStdlibImport) p = Just <$> mkStdlibImport
      | otherwise = return Nothing

    mkImport :: (Member (Reader Interval) r) => TopModulePath -> Sem r (Statement 'Parsed)
    mkImport _importModule = do
      _openModuleKw <- kw kwOpen
      _importKw <- kw kwImport
      return
        ( StatementImport
            Import
              { _importOpen =
                  Just
                    OpenModuleParams
                      { _openUsingHiding = Nothing,
                        _openPublicKw = Irrelevant Nothing,
                        _openPublic = NoPublic,
                        ..
                      },
                _importAsName = Nothing,
                ..
              }
        )

    mkStdlibImport :: (Member (Reader Interval) r) => Sem r (Statement 'Parsed)
    mkStdlibImport = do
      stdlibSymbol <- symbol "Stdlib"
      preludeSymbol <- symbol "Prelude"
      mkImport (mkTopModulePath (stdlibSymbol :| [preludeSymbol]))

    l :: Interval
    l = singletonInterval (mkInitialLoc (p ^. packageFile))

packageDescriptionDir' :: Path Abs Dir
packageDescriptionDir' =
  $( FE.makeRelativeToProject (toFilePath packageDescriptionDir)
       >>= runIO
         . parseAbsDir
       >>= lift
   )
