module MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Base where

import qualified MiniJuvix.Syntax.MicroJuvix.Pretty as M
import Prettyprinter
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Error.Types
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.Concrete.Language (getLoc)

data Eann = Highlight
  | MicroAnn M.Ann

ppCode :: M.PrettyCode c => c -> Doc Eann
ppCode = reAnnotate MicroAnn . M.runPrettyCode M.defaultOptions

indent' :: Doc ann -> Doc ann
indent' = indent 2

prettyT :: Text -> Doc Eann
prettyT = pretty

highlight :: Doc Eann -> Doc Eann
highlight = annotate Highlight

class PrettyError e where
  ppError :: e -> Doc Eann

instance PrettyError WrongConstructorType where
  ppError e = "Type error near" <+> highlight (pretty (funName ^. nameDefined)) <+> "in the definition of" <+> highlight (pretty funName) <> "."
    <> line <> "The constructor" <+> (ppCode (e ^. wrongCtorTypeName)) <+> "has type:"
    <> line <> indent' (ppCode (e ^. wrongCtorTypeActual))
    <> line <> "but is expected to have type:"
    <> line <> indent' (ppCode (e ^. wrongCtorTypeExpected))
    where
      funName :: Name
      funName = e ^. wrongCtorTypeFunname

instance PrettyError WrongConstructorAppArgs where
  ppError e = "Type error near" <+> highlight (pretty (funName ^. nameDefined)) <+> "in the definition of" <+> highlight (pretty (funName ^. nameText)) <> "."
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
      funName :: Name
      funName = e ^. wrongCtorAppName

instance PrettyError WrongType where
  ppError e = "Type error near" <+> highlight (pretty (getLoc subjectExpr)) <> "."
    <> line <> "The expression" <+> ppCode subjectExpr <+> "has type:"
    <> line <> indent' (ppCode (e ^. wrongTypeInferredType))
    <> line <> "but is expected to have type:"
    <> line <> indent' (ppCode (e ^. wrongTypeExpectedType))
    where
      subjectExpr :: Expression
      subjectExpr = e ^. wrongTypeExpression

instance PrettyError ExpectedFunctionType where
  ppError e = "Type error near" <+> highlight (pretty (getLoc subjectExpr)) <> "."
    <> line <> "In the expression:"
    <> line <> indent' (ppCode (e ^. expectedFunctionTypeExpression))
    <> line <> "the expression" <+> ppCode (e ^. expectedFunctionTypeApp) <+> "is expected to have a function type but has type:"
    <> line <> indent' (ppCode (e ^. expectedFunctionTypeType))
    where
      subjectExpr :: Expression
      subjectExpr = e ^. expectedFunctionTypeExpression

instance PrettyError TooManyPatterns where
  ppError e = "Type error near" <+> highlight (pretty (name ^. nameDefined))
    <> line <> "In in the definition of" <+> ppCode name <+> "the function clause:"
    <> line <> indent' (ppCode (e ^. tooManyPatternsClause))
    <> line <> "matches too many patterns. It should match the following types:"
    <> line <> indent' (hsep (ppCode <$> (e ^. tooManyPatternsTypes)))

    where
      name :: Name
      name =  (e ^. tooManyPatternsClause . clauseName)
