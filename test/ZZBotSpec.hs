{-# LANGUAGE ScopedTypeVariables #-}

module ZZBotSpec (spec) where

import Control.Monad
import Test.Hspec
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import Paths_zzbot

type Args = [String]

-- | Executes zzbot on the given arguments, returning exit code and stdout
zzBot :: FilePath -> Args -> IO (ExitCode, String, String)
zzBot file args =
  readProcessWithExitCode "zzbot" (file:args) ""

zzBotOn :: String -> Args -> IO (ExitCode, String, String)
zzBotOn fileContent args =
  withSystemTempFile "zzprintout" $ \tmp hfile -> do
    hPutStr hfile fileContent
    hFlush hfile
    zzBot tmp args

-- | Executes zzbot on the given arguments, returning the exit code
zzRc :: FilePath -> Args -> IO ExitCode
zzRc file args = do
  (exitCode, _, _) <- zzBot file args
  return exitCode

shouldSucceedConfigs :: [String]
shouldSucceedConfigs =
  [ "configs/config.xml"
  , "configs/tests/db1.xml"
  , "configs/tests/db_nonull.xml"
  ]

shouldFailConfigs :: [String]
shouldFailConfigs =
  [ "configs/tests/haltOnFailure1.xml"
  , "configs/tests/haltOnFailure2.xml"
  , "configs/tests/haltOnFailure3.xml"
  ]

otherConfigs :: [String]
otherConfigs =
  [ "configs/examples/kcg.xml" ]

spec = do
  describe "zzbot" $ do
    forM_ shouldSucceedConfigs $ \fileName -> do
      dataFileName <- runIO (getDataFileName fileName)
      it ("should succeed on " <> fileName) $
        zzRc dataFileName [] `shouldReturn` ExitSuccess
    forM_ shouldFailConfigs $ \fileName -> do
      dataFileName <- runIO (getDataFileName fileName)
      it ("should fail on " <> fileName) $
        zzRc dataFileName [] `shouldNotReturn` ExitSuccess
  describe "zzbot --print" $
    forM_ allConfigs $ \fileName -> do
      dataFileName <- runIO (getDataFileName fileName)
      it ("should succeed on " <> fileName) $
        zzRc dataFileName ["--print"] `shouldReturn` ExitSuccess
      it ("is idempotent on " <> fileName) $ do
        res1@(_, stdout1, _) <- zzBot dataFileName ["--print"]
        res2 <- zzBotOn stdout1 ["--print"]
        res1 `shouldBe` res2
  where allConfigs :: [String] =
             shouldSucceedConfigs
          ++ shouldFailConfigs
          ++ otherConfigs
