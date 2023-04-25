module Juvix.Parser.Error where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Options (fromGenericOptions)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Extra.Paths
import Juvix.Prelude
import Text.Megaparsec qualified as M
import Text.Megaparsec.Error (errorOffset)

data ParserError
  = ErrMegaparsec MegaparsecError
  | ErrTopModulePath TopModulePathError
  | ErrWrongTopModuleName WrongTopModuleName
  | ErrStrinOrFile StdinOrFileError
  deriving stock (Show)

instance ToGenericError ParserError where
  genericError = \case
    ErrMegaparsec e -> genericError e
    ErrTopModulePath e -> genericError e
    ErrWrongTopModuleName e -> genericError e
    ErrStrinOrFile e -> genericError e

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
          _genericErrorMessage = AnsiText $ pretty @_ @AnsiStyle e,
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
          _genericErrorMessage = AnsiText msg,
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
