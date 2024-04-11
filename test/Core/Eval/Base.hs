module Core.Eval.Base where

import Base
import Data.Aeson
import Data.Aeson.BetterErrors
import Data.ByteString.Lazy qualified as B
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Read
import GHC.Base (seq)
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Evaluator hiding (doEval)
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Extra.Value
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Core.Translation.FromSource

data EvalMode
  = EvalModePlain
  | EvalModeJSON

data EvalData = EvalData
  { _evalDataInput :: [(Text, Text)],
    _evalDataOutput :: Text
  }
  deriving stock (Generic)

makeLenses ''EvalData

type JSONError = Text

instance FromJSON EvalData where
  parseJSON = toAesonParser id parseEvalData
    where
      parseEvalData :: Parse JSONError EvalData
      parseEvalData = do
        _evalDataInput <- filter (\p -> fst p /= "out") <$> (eachInObject asText)
        mout <- keyMay "out" asText
        let _evalDataOutput = fromMaybe "true" mout
        return EvalData {..}

coreEvalAssertion' ::
  EvalMode ->
  InfoTable ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
coreEvalAssertion' mode tab mainFile expectedFile step =
  length (fromText (ppPrint tab) :: String) `seq`
    case HashMap.lookup sym (tab ^. identContext) of
      Just node -> do
        d <- readEvalData (ii ^. identifierArgNames)
        case d of
          Left msg -> assertFailure ("Error reading expected file: " <> msg)
          Right EvalData {..} ->
            withTempDir'
              ( \dirPath -> do
                  let outputFile = dirPath <//> $(mkRelFile "out.out")
                  hout <- openFile (toFilePath outputFile) WriteMode
                  step "Evaluate"
                  let tyargs = typeArgs (lookupIdentifierInfo m sym ^. identifierType)
                      args = zipWith mkArg (tyargs ++ repeat mkDynamic') (map snd _evalDataInput)
                      node' = mkApps' node args
                  r' <- doEval' opts mainFile hout tab node'
                  case r' of
                    Left err -> do
                      hClose hout
                      assertFailure (prettyString err)
                    Right value -> do
                      unless
                        (Info.member kNoDisplayInfo (getInfo value))
                        (hPutStrLn hout (ppPrint (toValue tab value)))
                      hClose hout
                      actualOutput <- readFile outputFile
                      step "Compare expected and actual program output"
                      assertEqDiffText ("Check: EVAL output = " <> toFilePath expectedFile) actualOutput _evalDataOutput
              )
      Nothing -> assertFailure ("No main function registered in: " <> toFilePath mainFile)
  where
    sym = fromJust (tab ^. infoMain)
    ii = lookupIdentifierInfo m sym
    m = moduleFromInfoTable tab

    opts = case mode of
      EvalModePlain -> defaultEvalOptions
      EvalModeJSON -> defaultEvalOptions {_evalOptionsNoFailure = True}

    mkArg :: Type -> Text -> Node
    mkArg ty arg =
      let n = fst $ fromRight' $ decimal arg
       in case (isTypeBool ty, n) of
            (True, 0) -> mkConstr' (BuiltinTag TagFalse) []
            (True, _) -> mkConstr' (BuiltinTag TagTrue) []
            (False, _) -> mkConstant' (ConstInteger n)

    readEvalData :: [Maybe Text] -> IO (Either String EvalData)
    readEvalData argnames = case mode of
      EvalModePlain -> do
        expected <- readFile expectedFile
        return $
          Right $
            EvalData
              { _evalDataInput = [],
                _evalDataOutput = expected
              }
      EvalModeJSON -> do
        fmap
          ( over evalDataInput sortArgs
              . over evalDataOutput (<> "\n")
          )
          . eitherDecode
          <$> B.readFile (toFilePath expectedFile)
        where
          sortArgs :: [(Text, Text)] -> [(Text, Text)]
          sortArgs args = sortBy cmp args
            where
              cmp :: (Text, Text) -> (Text, Text) -> Ordering
              cmp (k1, _) (k2, _) = compare i1 i2
                where
                  i1 = fromJust $ elemIndex k1 argnames'
                  i2 = fromJust $ elemIndex k2 argnames'

              argnames' =
                if
                    | length args == 1 ->
                        [fromMaybe "in" (head (nonEmpty' argnames))]
                    | otherwise ->
                        zipWith (\n -> fromMaybe ("in" <> show n)) [1 .. length args] (argnames ++ repeat Nothing)

coreEvalAssertion ::
  Path Abs File ->
  Path Abs File ->
  [TransformationId] ->
  (InfoTable -> Assertion) ->
  (String -> IO ()) ->
  Assertion
coreEvalAssertion mainFile expectedFile trans testTrans step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (prettyString err)
    Right (_, Nothing) -> do
      step "Compare expected and actual program output"
      expected <- readFile expectedFile
      assertEqDiffText ("Check: EVAL output = " <> toFilePath expectedFile) "" expected
    Right (tabIni, Just node) ->
      case run $ runReader defaultCoreOptions $ runError $ applyTransformations trans $ moduleFromInfoTable $ setupMainFunction defaultModuleId tabIni node of
        Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
        Right m -> do
          let tab = computeCombinedInfoTable m
          assertBool "Check info table" (checkInfoTable tab)
          testTrans tab
          coreEvalAssertion' EvalModePlain tab mainFile expectedFile step

coreEvalErrorAssertion :: Path Abs File -> (String -> IO ()) -> Assertion
coreEvalErrorAssertion mainFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left _ -> assertBool "" True
    Right (_, Nothing) -> assertFailure "no error"
    Right (tab, Just node) -> do
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            hout <- openFile (toFilePath outputFile) WriteMode
            step "Evaluate"
            r' <- doEval mainFile hout tab node
            hClose hout
            case r' of
              Left _ -> assertBool "" True
              Right _ -> assertFailure "no error"
        )

parseFile :: Path Abs File -> IO (Either MegaparsecError (InfoTable, Maybe Node))
parseFile f = do
  s <- readFile f
  return (runParser f defaultModuleId mempty s)

doEval' ::
  EvalOptions ->
  Path Abs File ->
  Handle ->
  InfoTable ->
  Node ->
  IO (Either CoreError Node)
doEval' opts f hout tab node =
  catchEvalErrorIO defaultLoc (hEvalIO' opts hout stdin hout (tab ^. identContext) [] node)
  where
    defaultLoc = singletonInterval (mkInitialLoc f)

doEval ::
  Path Abs File ->
  Handle ->
  InfoTable ->
  Node ->
  IO (Either CoreError Node)
doEval = doEval' defaultEvalOptions
