module Juvix.Parser.Error where

import Commonmark qualified as MK
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Options (fromGenericOptions)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Extra.Paths
import Juvix.Prelude
import Text.Megaparsec qualified as M
import Text.Megaparsec.Error (errorOffset)
import Text.Parsec.Error qualified as P
import Text.Parsec.Pos qualified as P

data ParserError
  = ErrMegaparsec MegaparsecError
  | ErrCommonmark CommonmarkError
  | ErrTopModulePath TopModulePathError
  | ErrWrongTopModuleName WrongTopModuleName
  | ErrStdinOrFile StdinOrFileError
  | ErrDanglingJudoc DanglingJudoc
  deriving stock (Show)

instance ToGenericError ParserError where
  genericError = \case
    ErrMegaparsec e -> genericError e
    ErrCommonmark e -> genericError e
    ErrTopModulePath e -> genericError e
    ErrWrongTopModuleName e -> genericError e
    ErrStdinOrFile e -> genericError e
    ErrDanglingJudoc e -> genericError e

instance Pretty MegaparsecError where
  pretty (MegaparsecError b) = pretty (M.errorBundlePretty b)

instance HasLoc MegaparsecError where
  getLoc (MegaparsecError b) = singletonInterval (mkLoc offset sourcePos)
    where
      state :: M.PosState Text
      state = M.bundlePosState b
      offset = errorOffset (head (M.bundleErrors b))

      sourcePos :: M.SourcePos
      sourcePos =
        (snd . head . fst)
          (M.attachSourcePos errorOffset (M.bundleErrors b) state)

newtype MegaparsecError = MegaparsecError
  { _megaParsecError :: M.ParseErrorBundle Text Void
  }
  deriving stock (Show)

instance ToGenericError MegaparsecError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText $ pretty @_ @AnsiStyle e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc e

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

data StdinOrFileError = StdinOrFileError
  deriving stock (Show)

instance ToGenericError StdinOrFileError where
  genericError StdinOrFileError =
    return
      GenericError
        { _genericErrorLoc = singletonInterval (mkInitialLoc formatStdinPath),
          _genericErrorMessage = prettyError "Neither JUVIX_FILE_OR_PROJECT nor --stdin option is choosen",
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
