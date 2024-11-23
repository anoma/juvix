module Juvix.Compiler.Backend.Lean.Pretty.Base where

-- import Data.Text qualified as Text
import Juvix.Compiler.Backend.Lean.Language
--import Juvix.Compiler.Backend.Lean.Pretty.Keywords
import Juvix.Compiler.Backend.Lean.Pretty.Options
import Juvix.Data.CodeAnn

class PrettyCode c where
  ppCode :: (Members '[Reader Options, Input Comment] r) => c -> Sem r (Doc Ann)

doc :: (PrettyCode c) => Options -> [Comment] -> c -> Doc Ann
doc opts comments =
  run
    . runReader opts
    . runInputList comments
    . ppCode

ppCodeQuoted :: (HasAtomicity c, PrettyCode c, Members '[Reader Options, Input Comment] r) => c -> Sem r (Doc Ann)
ppCodeQuoted c
  | atomicity c == Atom = ppCode c
  | otherwise = parens <$> ppCode c

ppTopCode :: (HasAtomicity c, PrettyCode c, Members '[Reader Options, Input Comment] r) => c -> Sem r (Doc Ann)
ppTopCode c = parensIf (not (isAtomic c)) <$> ppCode c

ppComments :: (Member (Input Comment) r) => Interval -> Sem r (Doc Ann)
ppComments loc = do
  comments <- inputWhile cmpLoc
  return
    . mconcatMap (\c -> annotate AnnComment $ "-- " <> pretty (c ^. commentText) <> line)
    $ comments
  where
    cmpLoc :: Comment -> Bool
    cmpLoc c = c ^. commentInterval . intervalStart <= loc ^. intervalEnd

ppCodeWithComments :: (PrettyCode a, HasLoc a, Members '[Reader Options, Input Comment] r) => a -> Sem r (Doc Ann, Doc Ann)
ppCodeWithComments a = do
  comments <- ppComments (getLoc a)
  res <- ppCode a
  return (comments, res)

instance PrettyCode Name where
  ppCode = return . prettyName False

instance PrettyCode Expression where
  ppCode = \case
    Unit -> return $ primitive "unit"

instance PrettyCode Declaration where
  ppCode = \case
    Def name expr -> do
      nameDoc <- ppCode name
      exprDoc <- ppCode expr
      return $ annotate AnnKeyword "def" <+> nameDoc <+> "=" <+> align exprDoc
    Theorem name expr -> do
      nameDoc <- ppCode name
      exprDoc <- ppCode expr
      return $ annotate AnnKeyword "theorem" <+> nameDoc <+> "=" <+> align exprDoc

instance PrettyCode Module where
  ppCode Module {..} = do
    nameDoc <- ppCode _moduleName
    imports <- mapM ppCode _moduleImports
    decls <- mapM ppCode _moduleDeclarations
    let importSection = vsep $ map (annotate AnnKeyword "import" <+>) imports
        declSection = vsep $ punctuate line decls
    return $
      annotate AnnKeyword "module" <+> nameDoc
        <> line
        <> importSection
        <> line
        <> line
        <> declSection