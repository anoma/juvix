module Juvix.Compiler.Tree.Error where

import Juvix.Compiler.Tree.Language.Base
import Juvix.Data.PPOutput
import Text.Show

data TreeError = TreeError
  { _treeErrorLoc :: Maybe Location,
    _treeErrorMsg :: Text
  }

makeLenses ''TreeError

instance ToGenericError TreeError where
  genericError :: (Member (Reader GenericOptions) r) => TreeError -> Sem r GenericError
  genericError e = ask >>= generr
    where
      generr :: GenericOptions -> Sem r GenericError
      generr _ =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          i = getLoc e
          msg = pretty (e ^. treeErrorMsg)

instance Pretty TreeError where
  pretty (TreeError {..}) = pretty _treeErrorMsg

instance Show TreeError where
  show (TreeError {..}) = fromText _treeErrorMsg

instance HasLoc TreeError where
  getLoc (TreeError {..}) = fromMaybe defaultLoc _treeErrorLoc
    where
      defaultLoc :: Interval
      defaultLoc = singletonInterval (mkInitialLoc sourcePath)

      sourcePath :: Path Abs File
      sourcePath = $(mkAbsFile "/<tree>")
