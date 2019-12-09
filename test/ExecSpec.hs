{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ExecSpec (spec) where

import Config
import Control.Monad.Writer
import Data.Text.Prettyprint.Doc.Render.Terminal
import Exec
import System.Exit
import Test.Hspec

data LogEntry
  = Message Color String
  | StdOut String
  | StdErr String
  deriving (Eq, Show)

newtype MockExec a = MockExec (Writer [LogEntry] a)
  deriving (Functor, Applicative, Monad, MonadWriter [LogEntry])

runMockExec :: MockExec a -> (a, [LogEntry])
runMockExec (MockExec m) = runWriter m

instance MonadExec MockExec where

  zzLog color entry = tell [Message color entry]

  runShellCommand (Command "ls" ["a"]) = return (ExitSuccess, "foo bar", "")
  runShellCommand (Command "ls" ["b"]) = return (ExitSuccess, "bar baz", "")
  runShellCommand _ = return (ExitFailure 127, "", "command not found")

  putOutLn str = tell [StdOut str]

  putErrLn str = tell [StdErr str]

spec =
  describe "runBuild" $
    it "should log what it's doing" $
      runMockExec (runBuild testBuilder) `shouldBe` expectedOutput
  where
    testBuilder =
      Builder
        Nothing
        "test"
        [ ShellCmd Nothing (Command "ls" ["a"])
        , ShellCmd Nothing (Command "ls" ["b"])
        , ShellCmd Nothing (Command "some" ["junk"])
        ]
    expectedOutput =
      ( ExitFailure 127
      , [ Message Green "ls a"
        , StdOut "foo bar"
        , Message Green "ls b"
        , StdOut "bar baz"
        , Message Green "some junk"
        , StdErr "command not found"
        , Message Red "some junk failed: ExitFailure 127"
        ]
      )
