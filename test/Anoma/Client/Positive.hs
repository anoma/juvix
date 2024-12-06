module Anoma.Client.Positive where

import Anoma.Effect.Base
import Base
import Juvix.Compiler.Nockma.Language hiding (Path)
import Juvix.Compiler.Nockma.Translation.FromTree (anomaClosure)

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Anoma/Client/positive")

type Check =
  Sem
    '[ Reader [Term Natural],
       EmbedIO
     ]

data ClientTest = ClientTest
  { _clientTestNum :: Int,
    _clientTestTag :: Text,
    _clientRelRoot :: Path Rel Dir,
    _clientMainFile :: Path Rel File,
    _clientAssertion :: forall r. (Members '[Error SimpleError, Anoma, EmbedIO] r) => Term Natural -> Sem r ()
  }

makeLenses ''ClientTest

clientTestName :: ClientTest -> Text
clientTestName t = numberedTestName (t ^. clientTestNum) (t ^. clientTestTag)

withRootCopy :: (Path Abs Dir -> IO a) -> IO a
withRootCopy = withRootTmpCopy root

fromClientTest :: ClientTest -> TestTree
fromClientTest t = testCase (clientTestName t) assertion
  where
    assertion :: Assertion
    assertion = runM . runProcess . runSimpleErrorHUnit . ignoreLogger $ do
      res :: AnomaResult <- liftIO $ withRootCopy (compileMain False (t ^. clientRelRoot) (t ^. clientMainFile))
      let program :: Term Natural = (res ^. anomaClosure)
      p <- envAnomaPath
      runAnomaEphemeral p ((t ^. clientAssertion) program)

allTests :: TestTree
allTests =
  testGroup
    "Anoma Client positive tests"
    []
