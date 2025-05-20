module Juvix.Parser.Error
  ( module Juvix.Parser.Error,
    module Juvix.Parser.Error.Base,
  )
where

import Commonmark qualified as MK
import Data.Yaml qualified as Yaml
import Juvix.Compiler.Backend.Markdown.Error
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Options (fromGenericOptions)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Compiler.Pipeline.Loader.PathResolver.Error
import Juvix.Extra.Paths
import Juvix.Parser.Error.Base
import Juvix.Prelude
import Text.Megaparsec qualified as M
import Text.Parsec.Error qualified as P
import Text.Parsec.Pos qualified as P

data ParserError
  = ErrMegaparsec MegaparsecError
  | ErrSimpleParserError SimpleParserError
  | ErrCommonmark CommonmarkError
  | ErrWrongTopModuleName WrongTopModuleName
  | ErrWrongTopModuleNameOrphan WrongTopModuleNameOrphan
  | ErrStdinOrFile StdinOrFileError
  | ErrNamedApplicationMissingAt NamedApplicationMissingAt
  | ErrDanglingJudoc DanglingJudoc
  | ErrMarkdownBackend MarkdownBackendError
  | ErrFlatParseError FlatParseError
  | ErrExpectedDerivingInstance ExpectedDerivingInstanceError
  | ErrDerivingInstancePatterns DerivingInstancePatternsError
  | ErrYamlParseError YamlParseError
  | ErrCoercionNotAllowed CoercionNotAllowedError
  | ErrInstanceNotAllowed InstanceNotAllowedError
  | ErrExpectedInstance ExpectedInstanceError
  | ErrExpectedFunctionName ExpectedFunctionNameError
  | ErrExpectedResultType ExpectedResultTypeError
  | ErrExpectedColonEquals ExpectedColonEqualsError

instance FromSimpleParserError ParserError where
  fromSimpleParserError = ErrSimpleParserError

instance ToGenericError ParserError where
  genericError = \case
    ErrMegaparsec e -> genericError e
    ErrSimpleParserError e -> genericError e
    ErrCommonmark e -> genericError e
    ErrWrongTopModuleName e -> genericError e
    ErrWrongTopModuleNameOrphan e -> genericError e
    ErrStdinOrFile e -> genericError e
    ErrDanglingJudoc e -> genericError e
    ErrMarkdownBackend e -> genericError e
    ErrFlatParseError e -> genericError e
    ErrNamedApplicationMissingAt e -> genericError e
    ErrExpectedDerivingInstance e -> genericError e
    ErrDerivingInstancePatterns e -> genericError e
    ErrYamlParseError e -> genericError e
    ErrCoercionNotAllowed e -> genericError e
    ErrInstanceNotAllowed e -> genericError e
    ErrExpectedInstance e -> genericError e
    ErrExpectedFunctionName e -> genericError e
    ErrExpectedResultType e -> genericError e
    ErrExpectedColonEquals e -> genericError e

newtype ExpectedColonEqualsError = ExpectedColonEqualsError
  { _expectedColonEqualsErrorLoc :: Interval
  }
  deriving stock (Show)

instance HasLoc ExpectedColonEqualsError where
  getLoc ExpectedColonEqualsError {..} = _expectedColonEqualsErrorLoc

instance ToGenericError ExpectedColonEqualsError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = getLoc e,
          _genericErrorMessage = mkAnsiText ("Expected ':=' instead of '='" :: Text),
          _genericErrorIntervals = [getLoc e]
        }

newtype ExpectedResultTypeError = ExpectedResultTypeError
  { _expectedResultTypeErrorLoc :: Interval
  }
  deriving stock (Show)

instance HasLoc ExpectedResultTypeError where
  getLoc ExpectedResultTypeError {..} = _expectedResultTypeErrorLoc

instance ToGenericError ExpectedResultTypeError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = getLoc e,
          _genericErrorMessage = mkAnsiText ("Expected result type" :: Text),
          _genericErrorIntervals = [getLoc e]
        }

newtype ExpectedFunctionNameError = ExpectedFunctionNameError
  { _expectedFunctionNameErrorLoc :: Interval
  }
  deriving stock (Show)

instance HasLoc ExpectedFunctionNameError where
  getLoc ExpectedFunctionNameError {..} = _expectedFunctionNameErrorLoc

instance ToGenericError ExpectedFunctionNameError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = getLoc e,
          _genericErrorMessage = mkAnsiText ("Expected function name" :: Text),
          _genericErrorIntervals = [getLoc e]
        }

newtype ExpectedInstanceError = ExpectedInstanceError
  { _expectedInstanceErrorLoc :: Interval
  }
  deriving stock (Show)

instance HasLoc ExpectedInstanceError where
  getLoc ExpectedInstanceError {..} = _expectedInstanceErrorLoc

instance ToGenericError ExpectedInstanceError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = getLoc e,
          _genericErrorMessage = mkAnsiText ("Expected 'instance'" :: Text),
          _genericErrorIntervals = [getLoc e]
        }

newtype InstanceNotAllowedError = InstanceNotAllowedError
  { _instanceNotAllowedErrorLoc :: Interval
  }
  deriving stock (Show)

instance HasLoc InstanceNotAllowedError where
  getLoc InstanceNotAllowedError {..} = _instanceNotAllowedErrorLoc

instance ToGenericError InstanceNotAllowedError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = getLoc e,
          _genericErrorMessage = mkAnsiText ("'instance' not allowed here" :: Text),
          _genericErrorIntervals = [getLoc e]
        }

newtype CoercionNotAllowedError = CoercionNotAllowedError
  { _coercionNotAllowedErrorLoc :: Interval
  }
  deriving stock (Show)

instance HasLoc CoercionNotAllowedError where
  getLoc CoercionNotAllowedError {..} = _coercionNotAllowedErrorLoc

instance ToGenericError CoercionNotAllowedError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = getLoc e,
          _genericErrorMessage = mkAnsiText ("'coercion' not allowed here" :: Text),
          _genericErrorIntervals = [getLoc e]
        }

data YamlParseError = YamlParseError
  { _yamlParseError :: Yaml.ParseException,
    _yamlParseErrorLoc :: Interval
  }
  deriving stock (Show)

instance HasLoc YamlParseError where
  getLoc YamlParseError {..} = _yamlParseErrorLoc

instance ToGenericError YamlParseError where
  genericError e@YamlParseError {..} =
    return
      GenericError
        { _genericErrorLoc = getLoc e,
          _genericErrorMessage = mkAnsiText $ Yaml.prettyPrintParseException _yamlParseError,
          _genericErrorIntervals = [getLoc e]
        }

newtype DerivingInstancePatternsError = DerivingInstancePatternsError
  { _derivingInstancePatternsErrorLoc :: Interval
  }
  deriving stock (Show)

instance HasLoc DerivingInstancePatternsError where
  getLoc DerivingInstancePatternsError {..} = _derivingInstancePatternsErrorLoc

instance ToGenericError DerivingInstancePatternsError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = getLoc e,
          _genericErrorMessage = mkAnsiText ("Patterns not allowed for 'deriving instance'" :: Text),
          _genericErrorIntervals = [getLoc e]
        }

newtype ExpectedDerivingInstanceError = ExpectedDerivingInstanceError
  { _expectedDerivingInstanceErrorLoc :: Interval
  }
  deriving stock (Show)

instance HasLoc ExpectedDerivingInstanceError where
  getLoc ExpectedDerivingInstanceError {..} = _expectedDerivingInstanceErrorLoc

instance ToGenericError ExpectedDerivingInstanceError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = getLoc e,
          _genericErrorMessage = mkAnsiText ("Expected 'deriving instance'" :: Text),
          _genericErrorIntervals = [getLoc e]
        }

newtype FlatParseError = FlatParseError
  { _flatParseErrorLoc :: Interval
  }
  deriving stock (Show)

instance HasLoc FlatParseError where
  getLoc FlatParseError {..} = _flatParseErrorLoc

instance ToGenericError FlatParseError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = getLoc e,
          _genericErrorMessage = mkAnsiText ("FlatParse parsing error" :: Text),
          _genericErrorIntervals = [getLoc e]
        }

newtype CommonmarkError = CommonmarkError
  { _commonMarkError :: MK.ParseError
  }
  deriving stock (Show)

instance Pretty CommonmarkError where
  pretty (CommonmarkError e) =
    mconcat
      . intersperse line
      $ fmap (pretty . P.messageString) (P.errorMessages e)

instance HasLoc CommonmarkError where
  getLoc (CommonmarkError b) = singletonInterval (mkLoc 0 sourcePos)
    where
      sourcePos' :: P.SourcePos
      sourcePos' = P.errorPos b

      sourcePos :: M.SourcePos
      sourcePos =
        M.SourcePos
          { M.sourceName = P.sourceName sourcePos',
            M.sourceLine = M.mkPos $ P.sourceLine sourcePos',
            M.sourceColumn = M.mkPos $ P.sourceColumn sourcePos'
          }

instance ToGenericError CommonmarkError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText $ pretty @_ @AnsiStyle e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc e

data TopModulePathError = TopModulePathError
  { _topModulePathErrorPath :: TopModulePath,
    _topModulePathError :: PathResolverError
  }
  deriving stock (Show)

instance ToGenericError TopModulePathError where
  genericError TopModulePathError {..} = do
    let msg = ppCodeAnn _topModulePathError
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = getLoc _topModulePathErrorPath

data WrongTopModuleName = WrongTopModuleName
  { _wrongTopModuleNameExpectedPath :: Path Abs File,
    _wrongTopModuleNameActualPath :: Path Abs File,
    _wrongTopModuleNameActualName :: TopModulePath
  }
  deriving stock (Show)

instance ToGenericError WrongTopModuleName where
  genericError WrongTopModuleName {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc _wrongTopModuleNameActualName
          msg =
            "The top module"
              <+> ppCode opts' _wrongTopModuleNameActualName
              <+> "is defined in the file:"
                <> line
                <> pretty _wrongTopModuleNameActualPath
                <> line
                <> "But it should be in the file:"
                <> line
                <> pretty _wrongTopModuleNameExpectedPath

data WrongTopModuleNameOrphan = WrongTopModuleNameOrphan
  { _wrongTopModuleNameOrpahnExpectedName :: Text,
    _wrongTopModuleNameOrpahnActualName :: TopModulePath
  }
  deriving stock (Show)

instance ToGenericError WrongTopModuleNameOrphan where
  genericError WrongTopModuleNameOrphan {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc _wrongTopModuleNameOrpahnActualName
          msg =
            "This is a standalone module, but it's name is not the same as the file name."
              <> line
              <> "Expected module name:"
              <+> annotate (AnnKind KNameTopModule) (pcode _wrongTopModuleNameOrpahnExpectedName)
                <> line
                <> "Actual module name:"
              <+> ppCode opts' _wrongTopModuleNameOrpahnActualName

data StdinOrFileError = StdinOrFileError
  deriving stock (Show)

instance ToGenericError StdinOrFileError where
  genericError StdinOrFileError =
    return
      GenericError
        { _genericErrorLoc = singletonInterval (mkInitialLoc formatStdinPath),
          _genericErrorMessage = prettyError "Neither JUVIX_FILE_OR_PROJECT nor --stdin option is chosen",
          _genericErrorIntervals = []
        }

newtype DanglingJudoc = DanglingJudoc
  { _danglingJudoc :: Judoc 'Parsed
  }
  deriving stock (Show)

instance ToGenericError DanglingJudoc where
  genericError :: (Member (Reader GenericOptions) r) => DanglingJudoc -> Sem r GenericError
  genericError DanglingJudoc {..} = do
    opts <- fromGenericOptions <$> ask
    let msg = "Dangling judoc comment:\n" <+> ppCode opts _danglingJudoc
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = getLoc _danglingJudoc

data NamedApplicationMissingAt = NamedApplicationMissingAt
  { _namedApplicationMissingAtLoc :: Interval,
    _namedApplicationMissingAtFun :: Symbol,
    _namedApplicationMissingAtLhs :: FunctionLhs 'Parsed
  }

instance ToGenericError NamedApplicationMissingAt where
  genericError NamedApplicationMissingAt {..} = do
    opts <- fromGenericOptions <$> ask @GenericOptions
    let lhs :: FunctionLhs 'Parsed = _namedApplicationMissingAtLhs
        funWord :: Text
          | null (lhs ^. funLhsTypeSig . typeSigArgs) = "assignment"
          | otherwise = "function definition"
        fun' = ppCode opts _namedApplicationMissingAtFun
        msg :: Doc CodeAnn =
          "Unexpected "
            <> pretty funWord
            <+> ppCode opts _namedApplicationMissingAtLhs
            <+> kwAssign
              <> "\nPerhaps you intended to write a named application and missed the"
            <+> kwAt
            <+> "symbol? That would be something like"
              <> line
              <> fun'
              <> kwAt
              <> "{arg1 := ...; arg2 := ...; ... }"
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = _namedApplicationMissingAtLoc
