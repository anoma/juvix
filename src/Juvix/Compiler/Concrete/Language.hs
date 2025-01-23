module Juvix.Compiler.Concrete.Language
  ( module Juvix.Compiler.Concrete.Language.Base,
    module Juvix.Compiler.Concrete.Language.IsApeInstances,
    module Juvix.Compiler.Concrete.Language,
  )
where

import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Concrete.Language.Base
import Juvix.Compiler.Concrete.Language.IsApeInstances
import Juvix.Prelude

symbolTypeLabel :: forall s. (SingI s) => Lens' (SymbolType s) Text
symbolTypeLabel = case sing :: SStage s of
  SParsed -> withLocParam
  SScoped -> nameVerbatim

modulePathTypeLabel :: forall s t. (SingI s, SingI t) => ModulePathType s t -> Text
modulePathTypeLabel = case (sing :: SStage s, sing :: SModuleIsTop t) of
  (SParsed, SModuleTop) -> topModulePathToDottedPath
  (SParsed, SModuleLocal) -> (^. symbolTypeLabel)
  (SScoped, SModuleTop) -> (^. nameVerbatim)
  (SScoped, SModuleLocal) -> (^. symbolTypeLabel)

-- | Returns a label for the given statement. Useful so that we can refer to
-- statements from the CLI. This label may not be unique (e.g. for two import
-- statements that import the same module).
statementLabel :: forall s. (SingI s) => Statement s -> Maybe Text
statementLabel = \case
  StatementSyntax s -> goSyntax s
  StatementOpenModule {} -> Nothing
  StatementProjectionDef {} -> Nothing
  StatementFunctionDef f -> withFunctionSymbol Nothing (Just . (^. symbolTypeLabel)) (f ^. functionDefName)
  StatementDeriving f -> withFunctionSymbol Nothing (Just . (^. symbolTypeLabel)) (f ^. derivingFunLhs . funLhsName)
  StatementImport i -> Just (i ^. importModulePath . to modulePathTypeLabel)
  StatementInductive i -> Just (i ^. inductiveName . symbolTypeLabel)
  StatementModule i -> Just (i ^. modulePath . to modulePathTypeLabel)
  StatementAxiom a -> Just (a ^. axiomName . symbolTypeLabel)
  where
    goSyntax :: SyntaxDef s -> Maybe Text
    goSyntax = \case
      SyntaxFixity f -> Just (f ^. fixitySymbol . symbolTypeLabel)
      SyntaxOperator {} -> Nothing
      SyntaxIterator f -> Just (f ^. iterSymbol . symbolTypeLabel)
      SyntaxAlias f -> Just (f ^. aliasDefName . symbolTypeLabel)

-- | Indexes top statements by label
topStatementsByLabel :: (SingI s) => Module s t -> HashMap Text (Statement s)
topStatementsByLabel m =
  hashMap
    [ (lbl, x) | x <- m ^. moduleBody, Just lbl <- [statementLabel x]
    ]
