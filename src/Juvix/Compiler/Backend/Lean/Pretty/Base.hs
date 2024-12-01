module Juvix.Compiler.Backend.Lean.Pretty.Base where

import Juvix.Compiler.Backend.Lean.Language
import Juvix.Compiler.Backend.Lean.Pretty.Keywords
import Juvix.Compiler.Backend.Lean.Pretty.Options
import Juvix.Data.CodeAnn hiding (kwInductive)

arrow :: Doc Ann
arrow = "→"

class PrettyCode c where
  ppCode :: (Members '[Reader Options, Input Comment] r) => c -> Sem r (Doc Ann)

doc :: (PrettyCode c) => Options -> [Comment] -> c -> Doc Ann
doc opts comments =
  run . runReader opts . runInputList comments . ppCode

ppCodeQuoted :: (HasAtomicity c, PrettyCode c, Members '[Reader Options, Input Comment] r) => c -> Sem r (Doc Ann)
ppCodeQuoted c 
  | atomicity c == Atom = ppCode c
  | otherwise = parens <$> ppCode c

ppTopCode :: (HasAtomicity c, PrettyCode c, Members '[Reader Options, Input Comment] r) => c -> Sem r (Doc Ann)
ppTopCode c = parensIf (not (isAtomic c)) <$> ppCode c

ppComments :: (Member (Input Comment) r) => Interval -> Sem r (Doc Ann)
ppComments loc = do
  comments <- inputWhile cmpLoc
  return $ mconcatMap formatComment comments
  where
    cmpLoc :: Comment -> Bool
    cmpLoc c = c ^. commentInterval . intervalStart <= loc ^. intervalEnd
    
    formatComment :: Comment -> Doc Ann
    formatComment c = annotate AnnComment $ "-- " <> pretty (c ^. commentText) <> line

instance PrettyCode Name where
  ppCode = return . prettyName False

instance PrettyCode Literal where
  ppCode = \case
    LitNumeric n -> return $ annotate AnnLiteralInteger (pretty n)
    LitScientific s -> return $ annotate AnnLiteralInteger (pretty s)
    LitString s -> return $ annotate AnnLiteralString $ dquotes $ pretty s
    LitChar c -> return $ annotate AnnLiteralString $ squotes $ pretty c
    LitName n -> return $ annotate (AnnKind KNameConstructor) $ pretty n

instance PrettyCode Expression where
  ppCode = \case
    ExprVar idx -> return $ "x" <> pretty idx
    ExprSort lvl -> case lvl of
      LevelZero -> return "Type"
      _ -> do
        lvlDoc <- ppCode lvl
        return $ "Type" <+> lvlDoc
    ExprConst name levels -> do
      nameDoc <- ppCode name
      if null levels 
        then return nameDoc
        else do
          levelsDoc <- mapM ppCode levels
          return $ nameDoc <> brackets (hsep $ punctuate comma levelsDoc)
    ExprApp app -> ppCode app
    ExprLambda lam -> ppCode lam
    ExprPi piType -> ppCode piType
    ExprLet letE -> ppCode letE
    ExprLiteral lit -> ppCode (lit ^. withLocParam)
    ExprMatch case_ -> ppCode case_
    ExprHole _ -> return kwHole
    ExprMeta name -> ppCode name
    ExprQuote expr -> do
      quotedDoc <- ppCode expr
      return $ kwQuote <+> quotedDoc
    ExprStruct struct -> ppCode struct
    ExprArray elems -> do
      elemsDoc <- mapM ppCode elems
      return $ brackets $ hsep $ punctuate comma elemsDoc
    ExprProj proj -> ppCode proj
    ExprIf ifExpr -> ppCode ifExpr

instance PrettyCode Level where
  ppCode = \case
    LevelZero -> return $ primitive "0"
    LevelSucc lvl -> do
      lvlDoc <- ppCode lvl
      return $ lvlDoc <> annotate AnnKeyword "+" <> "1"
    LevelMax l1 l2 -> do
      l1Doc <- ppCode l1
      l2Doc <- ppCode l2
      return $ "max" <+> l1Doc <+> l2Doc
    LevelIMax l1 l2 -> do
      l1Doc <- ppCode l1
      l2Doc <- ppCode l2
      return $ "imax" <+> l1Doc <+> l2Doc
    LevelParam name -> ppCode name
    LevelMeta idx -> return $ primitive "meta" <> pretty idx

instance PrettyCode Type where
  ppCode = \case
    TySort lvl -> case lvl of
      LevelZero -> return "Type"
      _ -> do
        lvlDoc <- ppCode lvl
        return $ "Type" <+> lvlDoc
    TyVar var -> ppCode var
    TyFun fun -> ppCode fun
    TyPi piType -> ppCode piType
    TyApp app -> ppCode app

instance PrettyCode TypeVar where
  ppCode TypeVar{..} = ppCode _typeVarName

instance PrettyCode FunType where
  ppCode FunType{..} = do
    left <- ppCodeQuoted _funTypeLeft
    right <- ppCode _funTypeRight
    return $ left <+> kwArrow <+> right

instance PrettyCode PiType where
  ppCode PiType{..} = do
    binderDoc <- ppCode _piTypeBinder
    bodyDoc <- ppCode _piTypeBody
    return $ binderDoc <+> kwArrow <+> bodyDoc

instance PrettyCode TypeApp where
  ppCode TypeApp{..} = do
    headDoc <- ppCode _typeAppHead
    argDoc <- ppCode _typeAppArg
    return $ headDoc <+> argDoc

instance PrettyCode Application where
  ppCode Application{..} = do
    left <- ppCode _appLeft
    right <- ppTopCode _appRight
    return $ left <+> right

instance PrettyCode Lambda where
  ppCode Lambda{..} = do
    binder <- ppCode _lambdaBinder
    body <- ppCode _lambdaBody
    return $ "fun" <+> binder <+> "=>" <+> indent 2 body

instance PrettyCode Let where
  ppCode Let{..} = do
    nameDoc <- ppCode _letName
    typeDoc <- maybe (return Nothing) (fmap Just . ppCode) _letType
    valDoc <- ppCode _letValue
    bodyDoc <- ppCode _letBody
    let header = "let" <+> nameDoc <+?> fmap (kwColon <+>) typeDoc
    return $ vsep 
      [ header <+> ":="
      , indent 2 valDoc
      , "in" <+> bodyDoc
      ]

instance PrettyCode Binder where
  ppCode Binder{..} = do
    nameDoc <- ppCode _binderName
    typeDoc <- maybe 
      (return nameDoc)  -- Just the name when no type
      (\t -> do
         tDoc <- ppCode t
         return $ nameDoc <+> kwColon <+> tDoc)
      _binderType
    let wrapper = case _binderInfo of
          BinderDefault -> parens
          BinderImplicit -> braces
          BinderStrictImplicit -> doubleBraces
          BinderInstImplicit -> brackets
    return $ wrapper typeDoc

instance PrettyCode Case where
  ppCode Case{..} = do
    value <- ppCode _caseValue
    branches <- mapM ppCode (toList _caseBranches)
    case _caseValue of 
      ExprVar 0 -> return $ "fun" <> line <> indent 2 (vsep branches)
      _ -> return $ vsep
        [ "match" <+> value <+> "with"
        , indent 2 (vsep branches)
        ]

instance PrettyCode CaseBranch where
  ppCode CaseBranch{..} = do
    patternDoc <- ppCode _caseBranchPattern
    bodyDoc <- ppCode _caseBranchBody
    return $ "|" <+> patternDoc <+> "=>" <+> bodyDoc

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
      return $ vsep
        [ kwNamespace <+> nameDoc
        , indent' (vsep $ punctuate line cmdsDoc)
        , kwEnd <+> nameDoc
        ]
    ModSection name cmds -> do
      nameDoc <- ppCode name
      cmdsDoc <- mapM ppCode cmds
      return $ vsep
        [ kwSection <+> nameDoc
        , indent' (vsep cmdsDoc)
        , kwEnd <+> nameDoc
        ]
    ModSetOption name val -> do
      nameDoc <- ppCode name
      return $ kwSetOption <+> nameDoc <+> pretty val
    ModCommand cmd -> ppCode cmd

instance PrettyCode OpenNamespace where
  ppCode OpenNamespace{..} = do
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

instance PrettyCode Module where
  ppCode Module{..} = do
    importsDoc <- mapM ppCode _moduleImports
    commandsDoc <- mapM ppCode _moduleCommands
    return $ vsep
      [ vsep (punctuate line importsDoc)
      , vsep (punctuate line commandsDoc)
      ]

instance PrettyCode Definition where
  ppCode Definition{..} = do
    nameDoc <- ppCode _definitionName
    typeDoc <- maybe (return Nothing) (fmap Just . ppCode) _definitionType
    bodyDoc <- ppCode _definitionBody
    let header = kwDef <+> nameDoc <+?> fmap (kwColon <+>) typeDoc
    return $ vsep
      [ header
      , indent 2 $ kwAssign <+> bodyDoc
      ]

instance PrettyCode Axiom where
  ppCode Axiom{..} = do
    nameDoc <- ppCode _axiomName
    typeDoc <- ppCode _axiomType
    return $ kwAxiom <+> nameDoc <+> kwColon <+> typeDoc

instance PrettyCode Inductive where
  ppCode Inductive{..} = do
    nameDoc <- ppCode _inductiveName
    paramsDoc <- mapM ppCode _inductiveParams
    typeDoc <- ppCode _inductiveType
    ctorsDoc <- mapM (\(name, ty) -> do
      nameDoc <- ppCode name
      tyDoc <- ppCode ty
      return $ "|" <+> nameDoc <+> kwColon <+> tyDoc) _inductiveCtors
    return $ vsep
      [ kwInductive <+> nameDoc <+> hsep paramsDoc <+> kwColon <+> typeDoc
      , indent 2 (vsep ctorsDoc)
      , kwOpen <+> nameDoc
      ]

instance PrettyCode Structure' where
  ppCode Structure'{..} = do
    nameDoc <- ppCode _structureName
    paramsDoc <- mapM ppCode _structureParams
    fieldsDoc <- mapM (\(name, ty) -> do
      nameD <- ppCode name
      tyD <- ppCode ty
      return $ nameD <+> kwColon <+> tyD) _structureFields
    extendsDoc <- mapM ppCode _structureExtends
    let extendsClause = if null extendsDoc
                         then mempty
                         else kwExtends <+> hsep extendsDoc
    return $ vsep
      [ kwStructure <+> nameDoc <+> hsep paramsDoc <+> extendsClause
      , indent' (vsep fieldsDoc)
      ]

instance PrettyCode Class where
  ppCode Class{..} = do
    nameDoc <- ppCode _className
    paramsDoc <- mapM ppCode _classParams
    fieldsDoc <- mapM (\(name, ty) -> do
      nameD <- ppCode name
      tyD <- ppCode ty
      return $ nameD <+> kwColon <+> tyD) _classFields
    return $ vsep
      [ kwClass <+> nameDoc <+> hsep paramsDoc <+> kwWhere
      , indent' (vsep fieldsDoc)
      ]

instance PrettyCode Instance where
  ppCode Instance{..} = do
    nameDoc <- maybe (return mempty) ppCode _instanceName
    typeDoc <- ppCode _instanceType
    valueDoc <- ppCode _instanceValue
    priorityDoc <- return $ maybe mempty (\p -> kwAt <+> pretty p) _instancePriority
    return $ vsep
      [ kwInstance <+> nameDoc <+> kwColon <+> typeDoc <+> priorityDoc
      , indent' $ kwAssign <+> valueDoc
      ]

instance PrettyCode Attribute where
  ppCode Attribute{..} = do
    nameDoc <- ppCode _attrName
    argsDoc <- mapM ppCode _attrArgs
    return $ "[" <> nameDoc <+> hsep argsDoc <> "]"

instance PrettyCode Structure where
  ppCode Structure{..} = do
    baseDoc <- maybe (return mempty) ppCode _structBase
    fieldsDoc <- mapM (\(name, expr) -> do
      nameDoc <- ppCode name
      exprDoc <- ppCode expr
      return $ nameDoc <+> kwAssign <+> exprDoc) _structFields
    return $ braces $ align $ vsep $
      [baseDoc | not (null _structFields)] ++
      fieldsDoc

instance PrettyCode Projection where
  ppCode Projection{..} = do
    exprDoc <- ppCode _projExpr
    fieldDoc <- ppCode _projField
    return $ exprDoc <> kwDot <> fieldDoc

instance PrettyCode If where
  ppCode If{..} = do
    cond <- ppCode _ifCond
    thenBranch <- ppCode _ifThen
    elseBranch <- ppCode _ifElse
    return $ vsep
      [ kwIf <+> cond
      , indent' $ kwThen <+> thenBranch
      , indent' $ kwElse <+> elseBranch
      ]
