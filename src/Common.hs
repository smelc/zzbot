module Common where

import System.Exit

-- | The status of a build and a step, doc gives a few indications over the ordering's choice
data Status =
    Success -- ^ Build or step succeeded
  | Warning -- ^ Build or step yielded a warning
  | Cancellation -- ^ Build or step was cancelled, supersedes warnings
  | Failure -- ^ Build or step failed, supersedes cancellation
  | Error -- ^ Internal zzbot error, supersedes everything
  deriving (Eq, Ord, Show)

type BuildID = Int -- ^ The unique identifier of a build
type StepID = Int -- ^ The unique identifier of a step

toExitCode :: ExitCode -> Status
toExitCode ExitSuccess = Success
toExitCode _ = Failure

-- This type could be in LowLevelDb, but it would require clients of Db to import LowLevelDb
-- which I would rather not to (solely Db should depend on LowLevelDb)
data StepStreams = StepStreams
  { stdout :: Maybe String
  , stderr :: Maybe String
  }
  deriving (Eq, Ord, Show)
