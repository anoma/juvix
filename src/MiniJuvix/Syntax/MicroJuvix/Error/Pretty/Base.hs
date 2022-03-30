module MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Base where

import qualified MiniJuvix.Syntax.MicroJuvix.Pretty as M
import Prettyprinter
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Error.Types
import MiniJuvix.Syntax.MicroJuvix.Language

data Eann = Highlight
  | MicroAnn M.Ann

ppCode :: M.PrettyCode c => c -> Doc Eann
ppCode = reAnnotate MicroAnn . M.runPrettyCode M.defaultOptions

indent' :: Doc ann -> Doc ann
indent' = indent 2

class PrettyError e where
  ppError :: e -> Doc Eann

instance PrettyError WrongConstructorType where
  ppError e = "Type error during pattern matching."
    <> line <> "The constructor" <+> (ppCode (e ^. wrongCtorTypeName)) <+> "has type:"
    <> line <> indent' (ppCode (e ^. wrongCtorTypeActual))
    <> line <> "but is expected to have type:"
    <> line <> indent' (ppCode (e ^. wrongCtorTypeExpected))

prettyT :: Text -> Doc Eann
prettyT = pretty

instance PrettyError WrongConstructorAppArgs where
  ppError e = "Type error during pattern matching."
    <> line <> "The constructor:" <+> ctorName <+> "is being matched against" <+> numPats <> ":"
    <> line <> indent' (ppCode (e ^. wrongCtorAppApp))

    <> line <> "but is expected to be matched against" <+> numTypes <+> "with the following types:"
    <> line <> indent' (hsep (ctorName : (ppCode <$> (e ^. wrongCtorAppTypes))))
    where
      numPats :: Doc ann
      numPats = pat (length (e ^. wrongCtorAppApp . constrAppParameters))
      numTypes :: Doc ann
      numTypes = pat (length (e ^. wrongCtorAppTypes))
      ctorName :: Doc Eann
      ctorName = ppCode (e ^. wrongCtorAppApp . constrAppConstructor)
      pat :: Int -> Doc ann
      pat n = pretty n <+> plural "pattern" "patterns" n
