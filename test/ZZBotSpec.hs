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
zzBot :: FilePath -> Args -> IO (ExitCode, String)
zzBot file args = do
  (exitCode, stdout, _) <- readProcessWithExitCode "zzbot" (file:args) ""
  return (exitCode, stdout)

-- | Executes zzbot on the given arguments, returning the exit code
zzRc :: FilePath -> Args -> IO ExitCode
zzRc file args = do
  (exitCode, stdout) <- zzBot file args
  return exitCode

-- | Executes zzbot --print on a single file, then reexecute zzbot --print on the generated output
-- (if the initial execution succeeded)
zzPrintPrint :: FilePath -> IO ExitCode
zzPrintPrint file = do
  (exitCode, stdout) <- zzBot file ["--print"]
  case exitCode of
    ExitFailure _ -> return exitCode
    ExitSuccess ->
      join $ -- @polux: Is 'join' idiomatic?
        withSystemTempFile "zzprintout" $ \tmp hfile -> do
          hPutStr hfile stdout
          return $ zzRc tmp ["--print"]

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
  describe "zzbot --print is idempotent" $
    forM_ allConfigs $ \fileName -> do
      dataFileName <- runIO (getDataFileName fileName)
      it ("should succeed on " <> fileName) $
        zzPrintPrint dataFileName `shouldReturn` ExitSuccess
  where allConfigs :: [String] =
             shouldSucceedConfigs
          ++ shouldFailConfigs
          ++ otherConfigs  
