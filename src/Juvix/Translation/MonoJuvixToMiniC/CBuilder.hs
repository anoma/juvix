module Juvix.Translation.MonoJuvixToMiniC.CBuilder where

import Juvix.Internal.Strings qualified as Str
import Juvix.Prelude
import Juvix.Syntax.MiniC.Language
import Juvix.Translation.MonoJuvixToMiniC.CNames

namedArgs :: (Text -> Text) -> [CDeclType] -> [Declaration]
namedArgs prefix = zipWith namedCDecl argLabels
  where
    argLabels :: [Text]
    argLabels = prefix . show <$> [0 :: Integer ..]

namedDecl :: Text -> Bool -> DeclType -> Declaration
namedDecl n isPtr typ =
  Declaration
    { _declType = typ,
      _declIsPtr = isPtr,
      _declName = Just n,
      _declInitializer = Nothing
    }

namedCDecl :: Text -> CDeclType -> Declaration
namedCDecl n CDeclType {..} = namedDecl n _typeIsPtr _typeDeclType

declFunctionType :: DeclType
declFunctionType = DeclTypeDefType Str.juvixFunctionT

declFunctionPtrType :: CDeclType
declFunctionPtrType =
  CDeclType
    { _typeDeclType = declFunctionType,
      _typeIsPtr = True
    }

funPtrType :: CFunType -> CDeclType
funPtrType CFunType {..} =
  CDeclType
    { _typeDeclType =
        DeclFunPtr
          ( FunPtr
              { _funPtrReturnType = _cFunReturnType ^. typeDeclType,
                _funPtrIsPtr = _cFunReturnType ^. typeIsPtr,
                _funPtrArgs = _cFunArgTypes
              }
          ),
      _typeIsPtr = False
    }

mallocSizeOf :: Text -> Expression
mallocSizeOf typeName =
  functionCall (ExpressionVar Str.malloc) [functionCall (ExpressionVar Str.sizeof) [ExpressionVar typeName]]

juvixFunctionCall :: CFunType -> Expression -> [Expression] -> Expression
juvixFunctionCall funType funParam args =
  functionCall (castToType (funPtrType fTyp) (memberAccess Pointer funParam "fun")) (funParam : args)
  where
    fTyp :: CFunType
    fTyp = funType {_cFunArgTypes = declFunctionPtrType : (funType ^. cFunArgTypes)}

cFunTypeToFunSig :: Text -> CFunType -> FunctionSig
cFunTypeToFunSig name CFunType {..} =
  FunctionSig
    { _funcReturnType = _cFunReturnType ^. typeDeclType,
      _funcIsPtr = _cFunReturnType ^. typeIsPtr,
      _funcQualifier = None,
      _funcName = name,
      _funcArgs = namedArgs asFunArg _cFunArgTypes
    }
