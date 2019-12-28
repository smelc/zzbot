{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ExecSpec (spec) where

import Config
import Control.Monad.Writer
import Control.Monad.Except
import Data.Text.Prettyprint.Doc.Render.Terminal
import Exec
import System.Exit
import Test.Hspec

-- Logging mock exec

data LogEntry
  = Message LogLevel String
  | StdOut String
  | StdErr String
  deriving (Eq, Show)

newtype LoggingMockExec a = LoggingMockExec (Writer [LogEntry] a)
  deriving (Functor, Applicative, Monad, MonadWriter [LogEntry])

runLoggingMockExec :: LoggingMockExec a -> (a, [LogEntry])
runLoggingMockExec (LoggingMockExec m) = runWriter m

instance MonadExec LoggingMockExec where

  zzLog level entry = tell [Message level entry]

  runShellCommand _ (Command "ls" ["a"]) = return (ExitSuccess, "foo bar", "")
  runShellCommand _ (Command "ls" ["b"]) = return (ExitSuccess, "bar baz", "")
  runShellCommand _ _ = return (ExitFailure 127, "", "command not found")

  putOutLn str = tell [StdOut str]

  putErrLn str = tell [StdErr str]


-- Tracing mock exec

data Execution = Execution (Maybe String) Command
  deriving (Eq, Show)

newtype TracingMockExec a = TracingMockExec (Writer [Execution] a)
  deriving (Functor, Applicative, Monad, MonadWriter [Execution])

runTracingMockExec :: TracingMockExec a -> (a, [Execution])
runTracingMockExec (TracingMockExec m) = runWriter m

instance MonadExec TracingMockExec where
  zzLog color entry = return ()
  runShellCommand workdir command = do
    tell [Execution workdir command]
    return (ExitSuccess, "", "")
  putOutLn str = return ()
  putErrLn str = return ()

-- Tests

spec =
  describe "runBuild" $ do
    it "should log what it's doing" $
      runLoggingMockExec (runExceptT (runBuild testBuilder)) `shouldBe` expectedOutput
    it "should set the working directory as specified" $
      runTracingMockExec (runExceptT (runBuild testBuilder)) `shouldBe` expectedTrace
  where
    testBuilder =
      Builder
        (Just "dir1")
        "test"
        [ ShellCmd Nothing (Command "ls" ["a"]) Nothing
        , ShellCmd (Just "dir2") (Command "ls" ["b"]) Nothing
        , ShellCmd Nothing (Command "some" ["junk"]) Nothing
        ]
    expectedOutput =
      ( Left (ExitFailure 3)
      , [ Message Info "ls a"
        , StdOut "foo bar"
        , Message Info "ls b"
        , StdOut "bar baz"
        , Message Info "some junk"
        , StdErr "command not found"
        , Message Error "some junk failed: ExitFailure 127"
        ]
      )
    expectedTrace =
      ( Right ()
      , [ Execution (Just "dir1") (Command "ls" ["a"])
        , Execution (Just "dir2") (Command "ls" ["b"])
        , Execution (Just "dir1") (Command "some" ["junk"])
        ]
      )
