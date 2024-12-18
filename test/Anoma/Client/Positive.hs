module Anoma.Client.Positive where

import Anoma.Client.Base
import Anoma.Effect
import Anoma.Effect.Intents.Verify
import Base
import Juvix.Compiler.Nockma.Language hiding (Path)
import Juvix.Compiler.Nockma.Translation.FromTree (anomaClosure)

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Anoma/Client")

data ClientTest = ClientTest
  { _clientTestTag :: Text,
    _clientRelRoot :: Path Rel Dir,
    _clientMainFile :: Path Rel File,
    _clientAssertion :: forall r. (Members '[Logger, Error SimpleError, Anoma, EmbedIO, TestStep] r) => Term Natural -> Sem r ()
  }

makeLenses ''ClientTest

withRootCopy :: (Path Abs Dir -> IO a) -> IO a
withRootCopy = withRootTmpCopy root

fromClientTest :: ClientTest -> TestTree
fromClientTest t = testCaseSteps (t ^. clientTestTag) assertion
  where
    assertion :: (Text -> IO ()) -> Assertion
    assertion stepFun = runM . runProcess . runSimpleErrorHUnit . ignoreLogger . runStep stepFun $ do
      step "Compiling"
      res :: AnomaResult <- liftIO $ withRootCopy (compileMain False (t ^. clientRelRoot) (t ^. clientMainFile))
      let program :: Term Natural = (res ^. anomaClosure)
      p <- envAnomaPath
      runAnomaEphemeral p ((t ^. clientAssertion) program)

-- | Run prove with the given arguments.
-- Returns the traces from the prove endpoint
prove ::
  (Members '[Logger, Error SimpleError, Anoma, EmbedIO, TestStep] r) =>
  Term Natural ->
  [Term Natural] ->
  Sem r RunNockmaResult
prove program proveArgs = do
  step "Proving"
  runNockma
    RunNockmaInput
      { _runNockmaProgram = program,
        _runNockmaArgs = proveArgs
      }

-- | Run prove with the given arguments and submit the result to the mempool.
-- Returns the traces from the prove endpoint
submit ::
  (Members '[Logger, Error SimpleError, Anoma, EmbedIO, TestStep] r) =>
  Term Natural ->
  Sem r ()
submit provedProgram = do
  step "Submitting transaction candidate"
  addTransaction
    AddTransactionInput
      { _addTransactionInputCandidate = provedProgram
      }

isListUnrevealedCommitsAvailable :: ListUnrevealedCommitsResult -> Bool
isListUnrevealedCommitsAvailable l = not (null (l ^. listUnrevealedCommitsResultCommits))

data SwapProveResult = SwapProveResult
  { _swapProveCommitment :: Term Natural,
    _swapProveTransaction :: Term Natural,
    _swapProveTransactionCandidate :: Term Natural
  }

makeLenses ''SwapProveResult

proveSwap :: (Members '[Logger, Error SimpleError, Anoma, EmbedIO, TestStep] r) => Term Natural -> Sem r SwapProveResult
proveSwap swapProgram = do
  proveRes <- prove swapProgram []
  case proveRes ^. runNockmaTraces of
    [commitment, tx] ->
      return
        SwapProveResult
          { _swapProveCommitment = commitment,
            _swapProveTransaction = tx,
            _swapProveTransactionCandidate = proveRes ^. runNockmaResult
          }
    _ -> throw (SimpleError "Could not parse output of Swap prove")

clientTests :: [ClientTest]
clientTests =
  [ ClientTest
      { _clientTestTag = "Submit swap transaction candidate",
        _clientRelRoot = $(mkRelDir "."),
        _clientMainFile = $(mkRelFile "Swap.juvix"),
        _clientAssertion = \program -> do
          resProve <- proveSwap program
          submit (resProve ^. swapProveTransactionCandidate)
          step "fetching unrevealed commits"
          resList <- pollForOutput 2000 isListUnrevealedCommitsAvailable listUnrevealedCommits
          case resList ^. listUnrevealedCommitsResultCommits of
            [listCommitment] ->
              liftIO $
                assertBool
                  "expected commitment from prove and list to be equal"
                  (nockmaEq (resProve ^. swapProveCommitment) listCommitment)
            _ ->
              throw
                ( SimpleError "Expected exactly one commitment to be traced by prove and one commitment to be listed by listUnrevealedCommitments"
                )
      },
    -- We cannot test invalid intents until https://github.com/anoma/anoma/issues/1676 is fixed
    ClientTest
      { _clientTestTag = "Verify swap transaction",
        _clientRelRoot = $(mkRelDir "."),
        _clientMainFile = $(mkRelFile "Swap.juvix"),
        _clientAssertion = \program -> do
          resProve <- proveSwap program
          step "verifying swap transaction"
          resVerify <- verify VerifyInput {_verifyIntent = resProve ^. swapProveTransaction}
          liftIO $ assertBool "expected swap transaction to be a valid intent" (resVerify ^. verifyResultValid)
      }
  ]

allTests :: TestTree
allTests =
  testGroup
    "Anoma Client positive tests"
    (map fromClientTest clientTests)
