module Reachability.Positive where

import Base
import Data.HashSet qualified as HashSet
import Juvix.Pipeline
import Juvix.Syntax.MicroJuvix.Language qualified as Micro
import Juvix.Syntax.MicroJuvix.MicroJuvixTypedResult qualified as Micro

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath,
    _stdlibMode :: StdlibMode,
    _file :: FilePath,
    _reachable :: HashSet String
  }

makeLenses ''PosTest

root :: FilePath
root = "tests/positive"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ \step -> do
            cwd <- getCurrentDirectory
            entryFile <- canonicalizePath _file
            let noStdlib = _stdlibMode == StdlibExclude
                entryPoint =
                  EntryPoint
                    { _entryPointRoot = cwd,
                      _entryPointNoTermination = False,
                      _entryPointNoStdlib = noStdlib,
                      _entryPointModulePaths = pure entryFile
                    }

            step "Pipeline up to reachability"
            p :: Micro.MicroJuvixTypedResult <- runIO (upToMicroJuvixReachability entryPoint)

            step "Check reachability results"
            let names = concatMap getNames (p ^. Micro.resultModules)
            mapM_ check names
        }
  where
    check n = assertBool ("unreachable not filtered: " ++ unpack n) (HashSet.member (unpack n) _reachable)

getNames :: Micro.Module -> [Text]
getNames m = concatMap getDeclName (m ^. (Micro.moduleBody . Micro.moduleStatements))
  where
    getDeclName :: Micro.Statement -> [Text]
    getDeclName = \case
      Micro.StatementInductive i -> [i ^. (Micro.inductiveName . Micro.nameText)]
      Micro.StatementFunction f -> [f ^. (Micro.funDefName . Micro.nameText)]
      Micro.StatementForeign {} -> []
      Micro.StatementAxiom ax -> [ax ^. (Micro.axiomName . Micro.nameText)]
      Micro.StatementInclude i -> getNames (i ^. Micro.includeModule)

allTests :: TestTree
allTests =
  testGroup
    "Reachability positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Reachability with modules"
      "Reachability"
      StdlibInclude
      "M.juvix"
      ( HashSet.fromList
          ["f", "g", "h", "Bool", "Maybe"]
      ),
    PosTest
      "Reachability with modules and standard library"
      "Reachability"
      StdlibInclude
      "N.juvix"
      ( HashSet.fromList
          ["test", "Unit"]
      )
  ]
