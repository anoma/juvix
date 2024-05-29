module Juvix.Compiler.Backend.Rust.Translation.FromReg
  ( module Juvix.Compiler.Backend.Rust.Translation.FromReg,
    module Juvix.Compiler.Backend.Rust.Data.Result,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Backend
import Juvix.Compiler.Backend.Rust.Data.Result
import Juvix.Compiler.Backend.Rust.Data.Result as Rust
import Juvix.Compiler.Backend.Rust.Language as Rust
import Juvix.Compiler.Backend.Rust.Pretty as Rust
import Juvix.Compiler.Backend.Rust.Translation.FromReg.Base
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Extra.Info qualified as Reg
import Juvix.Compiler.Reg.Language qualified as Reg
import Juvix.Prelude

fromReg :: Bool -> Limits -> Reg.InfoTable -> Rust.Result
fromReg isRiscZero lims tab =
  Rust.Result $ show $ Rust.ppOut (Rust.Options isRiscZero) $ Program [programFunction, mainFunction]
  where
    info :: Reg.ExtraInfo
    info = Reg.computeExtraInfo lims tab

    mainFunid :: Int
    mainFunid = getFUID info $ fromJust $ tab ^. Reg.infoMainFunction

    mainFunction :: Function
    mainFunction =
      Function
        { _functionName = "main",
          _functionAttributes = Nothing,
          _functionArguments = [],
          _functionReturnType = Nothing,
          _functionBody =
            [ stmtLet NotMut "result" (mkCall "program" [ExprVerbatim "&mut Memory::new()", mkInteger mainFunid, mkVec []]),
              StatementExpression (mkCall "println!" [mkString "{}", mkVar "result"]),
              StatementReturn (Return Nothing)
            ]
        }

    programFunction :: Function
    programFunction =
      Function
        { _functionName = "program",
          _functionAttributes = Just "allow(unused_mut, unused)",
          _functionArguments =
            [ FunctionArgument NotMut "mem" Memory,
              FunctionArgument Mut "funid" Word,
              FunctionArgument Mut "args" VecOfWord
            ],
          _functionReturnType = Just Word,
          _functionBody = programBody
        }

    programBody :: [Statement]
    programBody = funConstDecls ++ juvixFunctions

    funConstDecls :: [Statement]
    funConstDecls = map mkFunConstDecl funs
      where
        mkFunConstDecl :: Reg.FunctionInfo -> Statement
        mkFunConstDecl funInfo =
          StatementConst $
            ConstDecl
              { _constVariable = getFunctionIdent info (funInfo ^. Reg.functionSymbol),
                _constType = Word,
                _constValue = mkInteger (getFUID info (funInfo ^. Reg.functionSymbol))
              }

    juvixFunctions :: [Statement]
    juvixFunctions =
      [ StatementLoop $
          Loop
            { _loopLabel = Just "'program",
              _loopBody =
                [ StatementMatch $
                    Match
                      { _matchValue = mkVar "funid",
                        _matchBranches = map (fromRegFunction info) funs ++ [errBranch]
                      }
                ]
            }
      ]
      where
        errBranch =
          MatchBranch
            { _matchBranchPattern = Nothing,
              _matchBranchBody = [StatementExpression $ mkCall "panic!" [mkString "unknown function"]]
            }

    funs :: [Reg.FunctionInfo]
    funs = HashMap.elems (tab ^. Reg.infoFunctions)

fromRegFunction :: Reg.ExtraInfo -> Reg.FunctionInfo -> MatchBranch
fromRegFunction info funInfo =
  MatchBranch
    { _matchBranchPattern = Just $ mkVar $ getFunctionIdent info (funInfo ^. Reg.functionSymbol),
      _matchBranchBody = varDecls ++ fromRegCode info (funInfo ^. Reg.functionCode)
    }
  where
    varsNum :: Int
    varsNum = getLocalVarsNum info (funInfo ^. Reg.functionSymbol)

    varDecls :: [Statement]
    varDecls = map (\n -> stmtDecl Mut ("var_" <> show n) Word) [0 .. varsNum - 1]

fromRegCode :: Reg.ExtraInfo -> Reg.Code -> [Statement]
fromRegCode info = concatMap (fromRegInstr info)

fromRegInstr :: Reg.ExtraInfo -> Reg.Instruction -> [Statement]
fromRegInstr info = \case
  Reg.Nop ->
    []
  Reg.Binop x ->
    [fromBinaryOp x]
  Reg.Unop x ->
    [fromUnaryOp x]
  Reg.Cairo {} ->
    unsupported "Cairo builtin"
  Reg.Assign Reg.InstrAssign {..} ->
    stmtsAssign (mkVarRef _instrAssignResult) (fromValue _instrAssignValue)
  Reg.Trace {} ->
    unsupported "trace"
  Reg.Dump ->
    unsupported "dump"
  Reg.Failure {} ->
    unsupported "fail"
  Reg.Prealloc {} ->
    []
  Reg.Alloc x ->
    fromAlloc x
  Reg.AllocClosure x ->
    fromAllocClosure x
  Reg.ExtendClosure x ->
    fromExtendClosure x
  Reg.TailCall x ->
    fromTailCall x
  Reg.Call x ->
    fromCall x
  Reg.TailCallClosures x ->
    fromTailCallClosures x
  Reg.CallClosures x ->
    fromCallClosures x
  Reg.Return x ->
    fromReturn x
  Reg.Branch x ->
    fromBranch x
  Reg.Case x ->
    fromCase x
  Reg.Block Reg.InstrBlock {..} ->
    fromRegCode info _instrBlockCode
  where
    unsupported :: Text -> a
    unsupported x = error ("unsupported: " <> x)

    fromBinaryOp :: Reg.InstrBinop -> Statement
    fromBinaryOp Reg.InstrBinop {..} =
      stmtAssign
        (mkVarRef _instrBinopResult)
        ( mkCall
            (getBinaryOpName _instrBinopOpcode)
            [fromValue _instrBinopArg1, fromValue _instrBinopArg2]
        )

    getBinaryOpName :: Reg.BinaryOp -> Text
    getBinaryOpName = \case
      Reg.OpIntAdd -> "smallint_add"
      Reg.OpIntSub -> "smallint_sub"
      Reg.OpIntMul -> "smallint_mul"
      Reg.OpIntDiv -> "smallint_div"
      Reg.OpIntMod -> "smallint_mod"
      Reg.OpIntLt -> "smallint_lt"
      Reg.OpIntLe -> "smallint_le"
      Reg.OpEq -> "juvix_equal"
      Reg.OpStrConcat -> unsupported "strings"
      Reg.OpFieldAdd -> unsupported "field type"
      Reg.OpFieldSub -> unsupported "field type"
      Reg.OpFieldMul -> unsupported "field type"
      Reg.OpFieldDiv -> unsupported "field type"

    fromUnaryOp :: Reg.InstrUnop -> Statement
    fromUnaryOp Reg.InstrUnop {..} = case _instrUnopOpcode of
      Reg.OpShow -> unsupported "strings"
      Reg.OpStrToInt -> unsupported "strings"
      Reg.OpArgsNum ->
        stmtAssign
          (mkVarRef _instrUnopResult)
          (mkCall "mem.get_closure_largs" [fromValue _instrUnopArg])
      Reg.OpFieldToInt -> unsupported "field type"
      Reg.OpIntToField -> unsupported "field type"

    mkVarRef :: Reg.VarRef -> Text
    mkVarRef Reg.VarRef {..} = case _varRefGroup of
      Reg.VarGroupArgs -> "args[" <> show _varRefIndex <> "]"
      Reg.VarGroupLocal -> "var_" <> show _varRefIndex

    fromVarRef :: Reg.VarRef -> Expression
    fromVarRef = mkVar . mkVarRef

    fromValue :: Reg.Value -> Expression
    fromValue = \case
      Reg.ValConst c -> fromConst c
      Reg.CRef Reg.ConstrField {..} ->
        case _constrFieldMemRep of
          Reg.MemRepConstr ->
            mkCall
              "mem.get_constr_arg"
              [fromVarRef _constrFieldRef, mkInteger _constrFieldIndex]
          Reg.MemRepTag ->
            unsupported "MemRepTag"
          Reg.MemRepTuple ->
            unsupported "MemRepTuple"
          Reg.MemRepUnit ->
            unsupported "MemRepUnit"
          Reg.MemRepUnpacked {} ->
            fromVarRef _constrFieldRef
      Reg.VRef x -> fromVarRef x

    fromConst :: Reg.Constant -> Expression
    fromConst = \case
      Reg.ConstInt x -> mkCall "make_smallint" [mkInteger x]
      Reg.ConstField {} -> impossible
      Reg.ConstBool True -> mkVar "BOOL_TRUE"
      Reg.ConstBool False -> mkVar "BOOL_FALSE"
      Reg.ConstString {} -> unsupported "strings"
      Reg.ConstUnit -> mkVar "OBJ_UNIT"
      Reg.ConstVoid -> mkVar "OBJ_VOID"

    fromAlloc :: Reg.InstrAlloc -> [Statement]
    fromAlloc Reg.InstrAlloc {..} =
      case _instrAllocMemRep of
        Reg.MemRepConstr ->
          stmtsAssign
            (mkVarRef _instrAllocResult)
            ( mkCall
                "mem.alloc_constr"
                [ mkInteger (getUID info _instrAllocTag),
                  mkArray (map fromValue _instrAllocArgs)
                ]
            )
        Reg.MemRepTag ->
          unsupported "MemRepTag"
        Reg.MemRepTuple ->
          unsupported "MemRepTuple"
        Reg.MemRepUnit ->
          unsupported "MemRepUnit"
        Reg.MemRepUnpacked {} ->
          unsupported "MemRepUnpacked"

    fromAllocClosure :: Reg.InstrAllocClosure -> [Statement]
    fromAllocClosure Reg.InstrAllocClosure {..} =
      stmtsAssign
        (mkVarRef _instrAllocClosureResult)
        ( mkCall
            "mem.alloc_closure"
            [ mkVar (getFunctionIdent info _instrAllocClosureSymbol),
              mkArray (map fromValue _instrAllocClosureArgs),
              mkInteger (_instrAllocClosureExpectedArgsNum - length _instrAllocClosureArgs)
            ]
        )

    fromExtendClosure :: Reg.InstrExtendClosure -> [Statement]
    fromExtendClosure Reg.InstrExtendClosure {..} =
      stmtsAssign
        (mkVarRef _instrExtendClosureResult)
        ( mkCall
            "mem.extend_closure"
            [ fromVarRef _instrExtendClosureValue,
              mkArray (map fromValue _instrExtendClosureArgs)
            ]
        )

    fromCall :: Reg.InstrCall -> [Statement]
    fromCall Reg.InstrCall {..} =
      case _instrCallType of
        Reg.CallFun sym ->
          stmtsAssign
            (mkVarRef _instrCallResult)
            ( mkCall
                "program"
                [mkVar "mem", mkVar (getFunctionIdent info sym), mkVec (map fromValue _instrCallArgs)]
            )
        Reg.CallClosure vr ->
          stmtsBlock
            [ stmtLet Mut "cargs" (mkVec (map fromValue _instrCallArgs)),
              stmtLet NotMut "(cfunid, args)" (mkCall "mem.call_closure" [fromVarRef vr, mkVar "&cargs"]),
              stmtAssign
                (mkVarRef _instrCallResult)
                ( mkCall
                    "program"
                    [mkVar "mem", mkVar "cfunid", mkVar "args"]
                )
            ]

    fromTailCall :: Reg.InstrTailCall -> [Statement]
    fromTailCall Reg.InstrTailCall {..} =
      case _instrTailCallType of
        Reg.CallFun sym ->
          [ stmtAssign "args" (mkVec (map fromValue _instrTailCallArgs)),
            stmtAssign "funid" (mkVar (getFunctionIdent info sym)),
            StatementContinue
          ]
        Reg.CallClosure vr ->
          [ stmtAssign
              "funid"
              ( mkCall
                  "mem.call_closure"
                  [fromVarRef vr, mkArray ((map fromValue _instrTailCallArgs))]
              ),
            StatementContinue
          ]

    fromCallClosures :: Reg.InstrCallClosures -> [Statement]
    fromCallClosures Reg.InstrCallClosures {..} =
      stmtsAssign
        (mkVarRef _instrCallClosuresResult)
        ( mkCall
            "apply!"
            [ mkVar "program",
              mkVar "mem",
              fromVarRef _instrCallClosuresValue,
              mkVec (map fromValue _instrCallClosuresArgs)
            ]
        )

    fromTailCallClosures :: Reg.InstrTailCallClosures -> [Statement]
    fromTailCallClosures Reg.InstrTailCallClosures {..} =
      [ StatementExpression $
          mkCall
            "tapply!"
            [ mkVar "'program",
              mkVar "program",
              mkVar "mem",
              mkVar "funid",
              mkVar "args",
              fromVarRef _instrTailCallClosuresValue,
              mkVec (map fromValue _instrTailCallClosuresArgs)
            ]
      ]

    fromBranch :: Reg.InstrBranch -> [Statement]
    fromBranch Reg.InstrBranch {..} =
      stmtsIf (mkCall "word_to_bool" [fromValue _instrBranchValue]) br1 br2
      where
        br1 = fromRegCode info _instrBranchTrue
        br2 = fromRegCode info _instrBranchFalse

    fromCase :: Reg.InstrCase -> [Statement]
    fromCase Reg.InstrCase {..} = do
      case _instrCaseIndRep of
        Reg.IndRepStandard ->
          [ StatementMatch $
              Match
                { _matchValue = mkCall "mem.get_constr_tag" [fromValue _instrCaseValue],
                  _matchBranches = brs'
                }
          ]
        Reg.IndRepEnum ->
          error "unsupported constructor representation"
        Reg.IndRepEnumRecord ->
          error "unsupported constructor representation"
        Reg.IndRepEnumMaybe {} ->
          error "unsupported constructor representation"
        Reg.IndRepRecord ->
          error "unsupported constructor representation"
        Reg.IndRepUnit ->
          error "unsupported constructor representation"
        Reg.IndRepNewtype {} ->
          error "unsupported constructor representation"
        Reg.IndRepMixed ->
          error "unsupported constructor representation"
      where
        brs = map fromCaseBranch _instrCaseBranches
        def = case _instrCaseDefault of
          Nothing -> Nothing
          Just c -> Just $ fromRegCode info c
        brs' =
          brs
            ++ [ MatchBranch
                   { _matchBranchPattern = Nothing,
                     _matchBranchBody = fromMaybe [StatementExpression $ mkCall "panic!" [mkString "match failure"]] def
                   }
               ]

    fromCaseBranch :: Reg.CaseBranch -> MatchBranch
    fromCaseBranch Reg.CaseBranch {..} =
      MatchBranch
        { _matchBranchPattern = Just $ mkInteger (getUID info _caseBranchTag),
          _matchBranchBody = fromRegCode info _caseBranchCode
        }

    fromReturn :: Reg.InstrReturn -> [Statement]
    fromReturn Reg.InstrReturn {..} =
      [StatementReturn $ Return $ Just $ fromValue _instrReturnValue]
