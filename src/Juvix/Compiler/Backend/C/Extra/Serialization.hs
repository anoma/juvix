module Juvix.Compiler.Backend.C.Extra.Serialization where

import Juvix.Compiler.Backend.C.Language
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude hiding (Binary, Unary)
import Language.C qualified as C
import Language.C.Data.Ident qualified as C
import Language.C.Pretty qualified as P
import Language.C.Syntax
import Text.PrettyPrint.HughesPJ qualified as HP

encAngles :: HP.Doc -> HP.Doc
encAngles p = HP.char '<' HP.<> p HP.<> HP.char '>'

prettyText :: Text -> HP.Doc
prettyText = HP.text . unpack

prettyCpp :: Cpp -> HP.Doc
prettyCpp = \case
  CppIncludeFile i -> "#include" HP.<+> HP.doubleQuotes (prettyText i)
  CppIncludeSystem i -> "#include" HP.<+> encAngles (prettyText i)
  CppDefine Define {..} -> prettyDefine _defineName (prettyDefineBody _defineBody)
  CppDefineParens Define {..} -> prettyDefine _defineName (HP.parens (prettyDefineBody _defineBody))
  where
    prettyDefineBody :: Expression -> HP.Doc
    prettyDefineBody e = P.pretty (mkCExpr e)
    prettyDefine :: Text -> HP.Doc -> HP.Doc
    prettyDefine n body = "#define" HP.<+> prettyText n HP.<+> body

prettyAttribute :: Attribute -> HP.Doc
prettyAttribute = \case
  ExportName n -> attr "export_name" n
  ImportName n -> attr "import_name" n
  where
    attr :: Text -> Text -> HP.Doc
    attr n v = "__attribute__" HP.<> HP.parens (HP.parens (prettyText n HP.<> HP.parens (HP.doubleQuotes (prettyText v))))

prettyCCode :: CCode -> HP.Doc
prettyCCode = \case
  ExternalDecl decl -> P.pretty (CDeclExt (mkCDecl decl))
  ExternalAttribute a -> prettyAttribute a
  ExternalFuncSig funSig -> P.pretty (CDeclExt (mkCFunSig funSig))
  ExternalFunc fun -> P.pretty (CFDefExt (mkCFunDef fun))
  ExternalMacro m -> prettyCpp m
  Verbatim t -> prettyText t

serialize :: CCodeUnit -> Text
serialize = show . codeUnitDoc
  where
    codeUnitDoc :: CCodeUnit -> HP.Doc
    codeUnitDoc c = HP.vcat (map prettyCCode (c ^. ccodeCode))

goTypeDecl' :: CDeclType -> Declaration
goTypeDecl' CDeclType {..} =
  Declaration
    { _declType = _typeDeclType,
      _declIsPtr = _typeIsPtr,
      _declName = Nothing,
      _declInitializer = Nothing
    }

mkCDecl :: Declaration -> CDecl
mkCDecl Declaration {..} = case _declType of
  DeclFunPtr FunPtr {..} ->
    CDecl
      (mkDeclSpecifier _funPtrReturnType)
      [(Just declr, Nothing, Nothing)]
      C.undefNode
    where
      declr :: CDeclr
      declr = CDeclr (mkIdent <$> _declName) derivedDeclr Nothing [] C.undefNode
      derivedDeclr :: [CDerivedDeclr]
      derivedDeclr = CPtrDeclr [] C.undefNode : (funDerDeclr <> ptrDeclr)
      ptrDeclr :: [CDerivedDeclr]
      ptrDeclr = [CPtrDeclr [] C.undefNode | _funPtrIsPtr]
      funDerDeclr :: [CDerivedDeclr]
      funDerDeclr = [CFunDeclr (Right (funArgs, False)) [] C.undefNode]
      funArgs :: [CDecl]
      funArgs = mkCDecl . goTypeDecl' <$> _funPtrArgs
  DeclArray Array {..} ->
    CDecl
      (mkDeclSpecifier _arrayType)
      [(Just declr, initializer, Nothing)]
      C.undefNode
    where
      declr :: CDeclr
      declr = CDeclr (mkIdent <$> _declName) derivedDeclr Nothing [] C.undefNode
      derivedDeclr :: [CDerivedDeclr]
      derivedDeclr = [CArrDeclr [] (CArrSize False (CConst (CIntConst (cInteger _arraySize) C.undefNode))) C.undefNode]
      initializer :: Maybe CInit
      initializer = mkCInit <$> _declInitializer
  _ ->
    CDecl
      (CStorageSpec (CStatic C.undefNode) : mkDeclSpecifier _declType)
      [(Just declrName, initializer, Nothing)]
      C.undefNode
    where
      declrName :: CDeclr
      declrName = CDeclr (mkIdent <$> _declName) ptrDeclr Nothing [] C.undefNode
      ptrDeclr :: [CDerivedDeclarator C.NodeInfo]
      ptrDeclr = [CPtrDeclr [] C.undefNode | _declIsPtr]
      initializer :: Maybe CInit
      initializer = mkCInit <$> _declInitializer

mkCInit :: Initializer -> CInit
mkCInit = \case
  ExprInitializer e -> CInitExpr (mkCExpr e) C.undefNode
  ListInitializer l -> CInitList (map (\i -> ([], mkCInit i)) l) C.undefNode
  DesignatorInitializer ds -> CInitList (f <$> ds) C.undefNode
  where
    f :: DesigInit -> ([CDesignator], CInit)
    f DesigInit {..} = ([CMemberDesig (mkIdent _desigDesignator) C.undefNode], mkCInit _desigInitializer)

mkFunCommon :: FunctionSig -> ([CDeclSpec], CDeclr)
mkFunCommon FunctionSig {..} = (declSpec, declr)
  where
    declr :: CDeclr
    declr = CDeclr (Just (mkIdent _funcName)) derivedDeclr Nothing [] C.undefNode
    declSpec :: [CDeclSpec]
    declSpec = qualifier <> mkDeclSpecifier _funcReturnType
    qualifier :: [CDeclSpec]
    qualifier = if _funcQualifier == StaticInline then [CStorageSpec (CStatic C.undefNode), CFunSpec (CInlineQual C.undefNode)] else []
    derivedDeclr :: [CDerivedDeclr]
    derivedDeclr = funDerDeclr <> ptrDeclr
    ptrDeclr :: [CDerivedDeclr]
    ptrDeclr = [CPtrDeclr [] C.undefNode | _funcIsPtr]
    funDerDeclr :: [CDerivedDeclr]
    funDerDeclr = [CFunDeclr (Right (funArgs, False)) [] C.undefNode]
    funArgs :: [CDecl]
    funArgs = mkCDecl <$> _funcArgs

mkCFunSig :: FunctionSig -> CDecl
mkCFunSig s =
  let (declSpec, declr) = mkFunCommon s
   in CDecl declSpec [(Just declr, Nothing, Nothing)] C.undefNode

mkCFunDef :: Function -> CFunDef
mkCFunDef Function {..} =
  let (declSpec, declr) = mkFunCommon _funcSig
   in CFunDef declSpec declr [] statement C.undefNode
  where
    statement :: CStat
    statement = CCompound [] block C.undefNode
    block :: [CBlockItem]
    block = mkBlockItem <$> _funcBody

mkBlockItem :: BodyItem -> CBlockItem
mkBlockItem = \case
  BodyStatement s -> CBlockStmt (mkCStat s)
  BodyDecl d -> CBlockDecl (mkCDecl d)

mkCExpr :: Expression -> CExpr
mkCExpr = \case
  ExpressionAssign Assign {..} -> CAssign CAssignOp (mkCExpr _assignLeft) (mkCExpr _assignRight) C.undefNode
  ExpressionCast Cast {..} -> CCast (mkCDecl _castDecl) (mkCExpr _castExpression) C.undefNode
  ExpressionCall Call {..} -> CCall (mkCExpr _callCallee) (mkCExpr <$> _callArgs) C.undefNode
  ExpressionLiteral l -> case l of
    LiteralInt i -> CConst (CIntConst (cInteger i) C.undefNode)
    LiteralChar c -> CConst (CCharConst (cChar c) C.undefNode)
    LiteralString s -> CConst (CStrConst (cString (unpack s)) C.undefNode)
  ExpressionVar n -> CVar (mkIdent n) C.undefNode
  ExpressionBinary Binary {..} ->
    CBinary (mkBinaryOp _binaryOp) (mkCExpr _binaryLeft) (mkCExpr _binaryRight) C.undefNode
  ExpressionUnary Unary {..} ->
    CUnary (mkUnaryOp _unaryOp) (mkCExpr _unarySubject) C.undefNode
  ExpressionMember MemberAccess {..} ->
    CMember (mkCExpr _memberSubject) (mkIdent _memberField) (_memberOp == Pointer) C.undefNode
  ExpressionStatement stmt ->
    CStatExpr (mkCStat stmt) C.undefNode

mkCStat :: Statement -> CStat
mkCStat = \case
  StatementReturn me -> CReturn (mkCExpr <$> me) C.undefNode
  StatementIf If {..} ->
    CIf (mkCExpr _ifCondition) (mkCStat _ifThen) (mkCStat <$> _ifElse) C.undefNode
  StatementSwitch Switch {..} ->
    CSwitch
      (mkCExpr _switchCondition)
      (CCompound [] (map CBlockStmt (caseStmts ++ caseDefault)) C.undefNode)
      C.undefNode
    where
      caseStmts =
        map
          ( \Case {..} ->
              CCase
                (mkCExpr _caseValue)
                ( CCompound
                    []
                    [ CBlockStmt (mkCStat _caseCode),
                      CBlockStmt (CBreak C.undefNode)
                    ]
                    C.undefNode
                )
                C.undefNode
          )
          _switchCases
      caseDefault =
        maybe
          [CDefault (mkCStat (StatementExpr (macroVar "UNREACHABLE"))) C.undefNode]
          (\x -> [CDefault (mkCStat x) C.undefNode])
          _switchDefault
  StatementLabel Label {..} ->
    CLabel (mkIdent _labelName) (mkCStat _labelCode) [] C.undefNode
  StatementGoto Goto {..} ->
    CGoto (mkIdent _gotoLabel) C.undefNode
  StatementExpr e -> CExpr (Just (mkCExpr e)) C.undefNode
  StatementCompound ss -> CCompound [] (CBlockStmt . mkCStat <$> ss) C.undefNode

mkBinaryOp :: BinaryOp -> CBinaryOp
mkBinaryOp = \case
  Eq -> CEqOp
  Neq -> CNeqOp
  And -> CLndOp
  Or -> CLorOp
  Plus -> CAddOp

mkUnaryOp :: UnaryOp -> CUnaryOp
mkUnaryOp = \case
  Address -> CAdrOp
  Indirection -> CIndOp
  Negation -> CNegOp

mkDeclSpecifier :: DeclType -> [CDeclSpec]
mkDeclSpecifier = \case
  DeclTypeDefType typeDefName -> mkTypeDefTypeSpec typeDefName
  DeclTypeDef typ -> CStorageSpec (CTypedef C.undefNode) : mkDeclSpecifier typ
  DeclStructUnion StructUnion {..} -> mkStructUnionTypeSpec _structUnionTag _structUnionName _structMembers
  DeclEnum Enum {..} -> mkEnumSpec _enumName _enumMembers
  DeclJuvixClosure -> mkTypeDefTypeSpec Str.juvixFunctionT
  BoolType -> [CTypeSpec (CBoolType C.undefNode)]
  DeclFunPtr {} -> []
  DeclArray {} -> []

mkEnumSpec :: Maybe Text -> Maybe [Text] -> [CDeclSpec]
mkEnumSpec name members = [CTypeSpec (CEnumType enum C.undefNode)]
  where
    enum :: CEnum
    enum = CEnum (mkIdent <$> name) (fmap (map (\m -> (mkIdent m, Nothing))) members) [] C.undefNode

mkTypeDefTypeSpec :: Text -> [CDeclSpec]
mkTypeDefTypeSpec name = [CTypeSpec (CTypeDef (mkIdent name) C.undefNode)]

mkStructUnionTypeSpec :: StructUnionTag -> Maybe Text -> Maybe [Declaration] -> [CDeclSpec]
mkStructUnionTypeSpec tag name members = [CTypeSpec (CSUType struct C.undefNode)]
  where
    struct :: CStructUnion
    struct = CStruct cStructTag (mkIdent <$> name) memberDecls [] C.undefNode
    memberDecls :: Maybe [CDecl]
    memberDecls = fmap (map mkCDecl) members
    cStructTag = case tag of
      StructTag -> CStructTag
      UnionTag -> CUnionTag

mkIdent :: Text -> C.Ident
mkIdent t = C.Ident (unpack t) 0 C.undefNode
