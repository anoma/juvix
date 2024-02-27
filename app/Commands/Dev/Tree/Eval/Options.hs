module Commands.Dev.Tree.Eval.Options where

import CommonOptions
import Juvix.Prelude.Pretty
import Prelude (show)

data Evaluator
  = EvalEffectful
  | EvalRaw
  deriving stock (Eq, Bounded, Enum, Data)

defaultEvaluator :: Evaluator
defaultEvaluator = EvalEffectful

instance Show Evaluator where
  show = \case
    EvalEffectful -> "effectful"
    EvalRaw -> "raw"

instance Pretty Evaluator where
  pretty = CommonOptions.show

optEvaluator :: Parser Evaluator
optEvaluator =
  option
    (eitherReader parseEvaluator)
    ( long "evaluator"
        <> value defaultEvaluator
        <> metavar "EVALUATOR_NAME"
        <> completer (mkCompleter (return . compl))
        <> help "hint: use autocomplete"
    )
  where
    compl :: String -> [String]
    compl s = filter (isPrefixOf s) (map Prelude.show (allElements @Evaluator))

    parseEvaluator :: String -> Either String Evaluator
    parseEvaluator s =
      maybe
        (Left err)
        Right
        ( lookup
            s
            [(Prelude.show e, e) | e :: Evaluator <- allElements]
        )
      where
        err :: String
        err = "Invalid evaluator name. The available names are: " <> Prelude.show (allElements @Evaluator)

data TreeEvalOptions = TreeEvalOptions
  { _treeEvalInputFile :: AppPath File,
    _treeEvalEvaluator :: Evaluator
  }
  deriving stock (Data)

makeLenses ''TreeEvalOptions

parseTreeEvalOptions :: Parser TreeEvalOptions
parseTreeEvalOptions = do
  _treeEvalInputFile <- parseInputFile FileExtJuvixTree
  _treeEvalEvaluator <- optEvaluator
  pure TreeEvalOptions {..}
