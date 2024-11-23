module Juvix.Compiler.Backend.Lean.Pretty.Base where

-- import Data.Text qualified as Text
import Juvix.Compiler.Backend.Lean.Language
import Juvix.Compiler.Backend.Lean.Pretty.Keywords
import Juvix.Compiler.Backend.Lean.Pretty.Options
import Juvix.Data.CodeAnn

arrow :: Doc Ann
arrow = "→"

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

instance PrettyCode Name where
  ppCode = return . prettyName False

instance PrettyCode Literal where
  ppCode = \case
    LitNumeric n -> return $ annotate AnnLiteralInteger (pretty n)
    LitScientific s -> return $ annotate AnnLiteralInteger (pretty s)
    LitString s -> return $ annotate AnnLiteralString $ dquotes $ pretty s
    LitChar c -> return $ annotate AnnLiteralString $ squotes $ pretty c
    LitName n -> return $ annotate (AnnKind KNameConstructor) $ pretty n

instance PrettyCode Type where
  ppCode = \case
    TySort lvl -> ppCode lvl
    TyVar var -> ppCode var
    TyFun fun -> ppCode fun
    TyPi piType -> ppCode piType
    TyApp app -> ppCode app

instance PrettyCode Level where
  ppCode = \case
    LevelZero -> return $ primitive "0"
    LevelSucc lvl -> do
      lvlDoc <- ppCode lvl
      return $ lvlDoc <> annotate AnnKeyword "+" <> "1"
    LevelMax l1 l2 -> do
      l1Doc <- ppCode l1
      l2Doc <- ppCode l2
      return $ primitive "max" <+> l1Doc <+> l2Doc
    LevelIMax l1 l2 -> do
      l1Doc <- ppCode l1
      l2Doc <- ppCode l2
      return $ primitive "imax" <+> l1Doc <+> l2Doc
    LevelParam name -> ppCode name
    LevelMeta idx -> return $ primitive "meta" <> pretty idx

instance PrettyCode TypeVar where
  ppCode TypeVar {..} = ppCode _typeVarName

instance PrettyCode FunType where
  ppCode FunType {..} = do
    left <- ppCode _funTypeLeft
    right <- ppCode _funTypeRight
    return $ left <+> kwArrow <+> right

instance PrettyCode PiType where
  ppCode PiType {..} = do
    binderDoc <- ppCode _piTypeBinder
    bodyDoc <- ppCode _piTypeBody
    return $ parens binderDoc <+> kwArrow <+> bodyDoc

instance PrettyCode TypeApp where
  ppCode TypeApp {..} = do
    headDoc <- ppCode _typeAppHead
    argDoc <- ppCode _typeAppArg
    return $ headDoc <+> argDoc

instance PrettyCode Binder where
  ppCode Binder {..} = do
    nameDoc <- ppCode _binderName
    typeDoc <- maybe (return mempty) ppCode _binderType
    let delim = case _binderInfo of
          BinderDefault -> parens
          BinderImplicit -> braces
          BinderStrictImplicit -> doubleBraces
          BinderInstImplicit -> brackets
    return $ delim $ nameDoc <+> typeDoc

instance PrettyCode Pattern where
  ppCode = \case
    PatLit lit -> ppCode (lit ^. withLocParam)
    PatVar name -> ppCode name
    PatCtor name patterns -> do
      nameDoc <- ppCode name
      patternsDoc <- mapM ppCode patterns
      return $ nameDoc <+> hsep patternsDoc
    PatInaccessible expr -> braces <$> ppCode expr
    PatTyped pat ty -> do
      patDoc <- ppCode pat
      tyDoc <- ppCode ty
      return $ patDoc <+> kwColon <+> tyDoc
    PatAs name pat -> do
      nameDoc <- ppCode name
      patDoc <- ppCode pat
      return $ nameDoc <> kwAt <+> patDoc

instance PrettyCode Command where
  ppCode = \case
    CmdDefinition def -> ppCode def
    CmdAxiom ax -> ppCode ax
    CmdInductive ind -> ppCode ind
    CmdStructure str -> ppCode str
    CmdClass cls -> ppCode cls
    CmdInstance inst -> ppCode inst

instance PrettyCode ModuleCommand where
  ppCode = \case
    ModImport name -> do
      nameDoc <- ppCode name
      return $ kwImport <+> nameDoc
    ModOpen namespace -> ppCode namespace
    ModNamespace name cmds -> do
      nameDoc <- ppCode name
      cmdsDoc <- mapM ppCode cmds
      return $
        kwNamespace <+> nameDoc
          <> line
          <> indent' (vsep cmdsDoc)
          <> line
          <> kwEnd
    ModSection name cmds -> do
      nameDoc <- ppCode name
      cmdsDoc <- mapM ppCode cmds
      return $
        kwSection <+> nameDoc
          <> line
          <> indent' (vsep cmdsDoc)
          <> line
          <> kwEnd
    ModSetOption name val -> do
      nameDoc <- ppCode name
      return $ kwSetOption <+> nameDoc <+> pretty val
    ModCommand cmd -> ppCode cmd

instance PrettyCode OpenNamespace where
  ppCode OpenNamespace {..} = do
    nsDoc <- ppCode _openNamespace
    optionDoc <- case _openOption of
      OpenAll -> return mempty
      OpenOnly names -> do
        namesDoc <- mapM ppCode names
        return $ kwUsing <+> braces (hsep $ punctuate comma namesDoc)
      OpenHiding names -> do
        namesDoc <- mapM ppCode names
        return $ kwHiding <+> braces (hsep $ punctuate comma namesDoc)
      OpenRenaming renames -> do
        renamesDoc <- mapM (\(old, new) -> do
                              oldDoc <- ppCode old
                              newDoc <- ppCode new
                              return $ oldDoc <+> kwAs <+> newDoc) renames
        return $ kwRenaming <+> braces (hsep $ punctuate comma renamesDoc)
    return $ kwOpen <+> nsDoc <+> optionDoc

instance PrettyCode Attribute where
  ppCode Attribute {..} = do
    nameDoc <- ppCode _attrName
    argsDoc <- mapM ppCode _attrArgs
    return $ "[" <> nameDoc <+> hsep argsDoc <> "]"

instance PrettyCode Expression where
  ppCode = \case
    ExprVar idx -> return $ "x" <> pretty idx
    ExprSort lvl -> ppCode lvl
    ExprConst name levels -> do
      nameDoc <- ppCode name
      levelsDoc <- mapM ppCode levels
      return $ nameDoc <> brackets (hsep $ punctuate comma levelsDoc)
    ExprApp Application {..} -> do
      leftDoc <- ppCode _appLeft
      rightDoc <- ppCode _appRight
      return $ leftDoc <+> rightDoc
    ExprLambda Lambda {..} -> do
      binderDoc <- ppCode _lambdaBinder
      bodyDoc <- ppCode _lambdaBody
      return $ kwLambda <+> binderDoc <+> kwArrow <+> bodyDoc
    ExprPi piType -> ppCode piType
    ExprLet Let {..} -> do
      nameDoc <- ppCode _letName
      typeDoc <- maybe (return mempty) (\t -> (colon <+>) <$> ppCode t) _letType
      valueDoc <- ppCode _letValue
      bodyDoc <- ppCode _letBody
      return $
        kwLet <+> nameDoc <> typeDoc <+> kwEquals <+> valueDoc
          <> line
          <> kwIn <+> bodyDoc
    ExprLiteral lit -> ppCode (lit ^. withLocParam)
    ExprMatch Case {..} -> do
      valueDoc <- ppCode _caseValue
      branchesDoc <- mapM ppCode (toList _caseBranches)
      return $ kwMatch <+> valueDoc <> line <> indent' (vsep branchesDoc)
    ExprHole interval -> return $ kwHole
    ExprMeta name -> ppCode name
    ExprQuote expr -> do
      quotedDoc <- ppCode expr
      return $ kwQuote <+> quotedDoc
    ExprStruct Structure {..} -> do
      baseDoc <- case _structBase of
        Nothing -> return mempty
        Just b -> do
          bDoc <- ppCode b
          return $ braces (bDoc <+> "with")
      fieldsDoc <- mapM (\(n, e) -> do
                            nameDoc <- ppCode n
                            exprDoc <- ppCode e
                            return $ nameDoc <+> colon <+> exprDoc) _structFields
      return $ baseDoc <> hsep (punctuate comma fieldsDoc)
    ExprArray elems -> do
      elemsDoc <- mapM ppCode elems
      return $ brackets $ hsep (punctuate comma elemsDoc)
    ExprProj Projection {..} -> do
      exprDoc <- ppCode _projExpr
      fieldDoc <- ppCode _projField
      return $ exprDoc <> kwDot <> fieldDoc
    ExprIf If {..} -> do
      condDoc <- ppCode _ifCond
      thenDoc <- ppCode _ifThen
      elseDoc <- ppCode _ifElse
      return $
        kwIf <+> condDoc
          <> line
          <> kwThen <+> thenDoc
          <> line
          <> kwElse <+> elseDoc



instance PrettyCode Application where
  ppCode Application {..} = do
    left <- ppTopCode _appLeft
    right <- ppCode _appRight
    return $ parens $ left <+> right

instance PrettyCode Lambda where
  ppCode Lambda {..} = do
    binder <- ppCode _lambdaBinder
    body <- ppCode _lambdaBody
    return $ kwLambda <+> binder <+> kwArrow <+> body

instance PrettyCode Let where
  ppCode Let {..} = do
    nameDoc <- ppCode _letName
    typeDoc <- maybe (return Nothing) (fmap Just . ppCode) _letType
    valueDoc <- ppCode _letValue
    bodyDoc <- ppCode _letBody
    return $
      align $
        kwLet <+> nameDoc <+?> (fmap (kwColon <+>) typeDoc)
          <+> kwAssign
          <+> valueDoc
          <> line
          <> kwIn <+> bodyDoc

instance PrettyCode Case where
  ppCode Case {..} = do
    value <- ppCode _caseValue
    branches <- mapM ppCode (toList _caseBranches)
    return $
      align $
        kwCase <+> value <+> kwOf
          <> line
          <> indent' (vsep branches)

instance PrettyCode CaseBranch where
  ppCode CaseBranch {..} = do
    patternDoc <- ppCode _caseBranchPattern
    bodyDoc <- ppCode _caseBranchBody
    return $ patternDoc <+> kwArrow <+> bodyDoc

instance PrettyCode Structure where
  ppCode Structure {..} = do
    baseDoc <- maybe (return mempty) ppCode _structBase
    fieldsDoc <- mapM (\(name, expr) -> do
                          nameDoc <- ppCode name
                          exprDoc <- ppCode expr
                          return $ nameDoc <+> kwAssign <+> exprDoc) 
                      _structFields
    return $
      braces $
        align $
          baseDoc <> line <> vsep fieldsDoc

instance PrettyCode Projection where
  ppCode Projection {..} = do
    exprDoc <- ppCode _projExpr
    fieldDoc <- ppCode _projField
    return $ exprDoc <> kwDot <> fieldDoc

instance PrettyCode If where
  ppCode If {..} = do
    cond <- ppCode _ifCond
    thenBranch <- ppCode _ifThen
    elseBranch <- ppCode _ifElse
    return $
      align $
        kwIf <+> cond
          <+> kwThen <+> thenBranch
          <+> kwElse <+> elseBranch

instance PrettyCode Definition where
  ppCode Definition {..} = do
    nameDoc <- ppCode _definitionName
    typeDoc <- maybe (return mempty) (fmap Just . ppCode) _definitionType
    bodyDoc <- ppCode _definitionBody
    return $
      align $
        kwDef <+> nameDoc
          <+?> (Just $ kwColon <+> fromMaybe mempty typeDoc)
          <+> kwAssign <+> bodyDoc

instance PrettyCode Axiom where
  ppCode Axiom {..} = do
    nameDoc <- ppCode _axiomName
    typeDoc <- ppCode _axiomType
    return $ kwAxiom <+> nameDoc <+> kwColon <+> typeDoc

instance PrettyCode Inductive where
  ppCode Inductive {..} = do
    nameDoc <- ppCode _inductiveName
    paramsDoc <- mapM ppCode _inductiveParams
    typeDoc <- ppCode _inductiveType
    ctorsDoc <- mapM (\(name, ty) -> do
                         nameDoc <- ppCode name
                         tyDoc <- ppCode ty
                         return $ nameDoc <+> kwColon <+> tyDoc) 
                     _inductiveCtors
    return $
      align $
        kwInductive <+> nameDoc
          <+> hsep paramsDoc
          <+> kwColon <+> typeDoc
          <> line
          <> indent' (vsep ctorsDoc)

instance PrettyCode Structure' where
  ppCode Structure' {..} = do
    nameDoc <- ppCode _structureName
    paramsDoc <- mapM ppCode _structureParams
    fieldsDoc <- mapM (\(name, ty) -> do
                          nameD <- ppCode name
                          tyD <- ppCode ty
                          return $ nameD <+> kwColon <+> tyD) _structureFields
    extendsDoc <- mapM ppCode _structureExtends
    return $
      align $
        kwStructure <+> nameDoc
          <+> hsep paramsDoc
          <+> (if null extendsDoc then mempty else kwExtends <+> hsep extendsDoc)
          <> line
          <> indent' (vsep fieldsDoc)

instance PrettyCode Class where
  ppCode Class {..} = do
    nameDoc <- ppCode _className
    paramsDoc <- mapM ppCode _classParams
    fieldsDoc <- mapM (\(name, ty) -> do
                          nameD <- ppCode name
                          tyD <- ppCode ty
                          return $ nameD <+> kwColon <+> tyD) _classFields
    return $
      align $
        kwClass <+> nameDoc
          <+> hsep paramsDoc
          <> line
          <> indent' (vsep fieldsDoc)

instance PrettyCode Instance where
  ppCode Instance {..} = do
    nameDoc <- maybe (return mempty) ppCode _instanceName
    typeDoc <- ppCode _instanceType
    valueDoc <- ppCode _instanceValue
    priorityDoc <- return $ maybe mempty (\p -> kwAt <+> pretty p) _instancePriority
    return $
      align $
        kwInstance <+> nameDoc
          <+> kwColon <+> typeDoc
          <+> priorityDoc
          <+> kwAssign <+> valueDoc

instance PrettyCode Module where
  ppCode Module {..} = do
    importsDoc <- mapM ppCode _moduleImports
    commandsDoc <- mapM ppCode _moduleCommands
    return $
      align $
        vsep (punctuate line importsDoc)
          <> line
          <> vsep commandsDoc


