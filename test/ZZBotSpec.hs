module ZZBotSpec (spec) where

import Control.Monad
import Test.Hspec
import System.Exit
import System.Process
import Paths_zzbot

type Args = [String]

zzBot :: FilePath -> Args -> IO ExitCode
zzBot file args = do
  (exitCode, _, _) <- readProcessWithExitCode "zzbot" (file:args) ""
  return exitCode

shouldSucceedConfigs :: [String]
shouldSucceedConfigs =
  [ "configs/config.xml"
  , "configs/tests/haltOnFailure2.xml"
  , "configs/tests/haltOnFailure3.xml"
  , "configs/tests/db1.xml"
  , "configs/tests/db_nonull.xml"
  ]

shouldFailConfigs :: [String]
shouldFailConfigs =
  [ "configs/tests/haltOnFailure1.xml"
  ]

printShouldSucceedConfigs :: [String]
printShouldSucceedConfigs =
  [ "configs/config.xml"
  , "configs/examples/kcg.xml"
  ]

spec = do
  describe "zzbot" $ do
    forM_ shouldSucceedConfigs $ \fileName -> do
      dataFileName <- runIO (getDataFileName fileName)
      it ("should succeed on " <> fileName) $
        zzBot dataFileName [] `shouldReturn` ExitSuccess
    forM_ shouldFailConfigs $ \fileName -> do
      dataFileName <- runIO (getDataFileName fileName)
      it ("should fail on " <> fileName) $
        zzBot dataFileName [] `shouldNotReturn` ExitSuccess
  describe "zzbot --print" $
    forM_ printShouldSucceedConfigs $ \fileName -> do
      dataFileName <- runIO (getDataFileName fileName)
      it ("should succeed on " <> fileName) $
        zzBot dataFileName ["--print"] `shouldReturn` ExitSuccess
