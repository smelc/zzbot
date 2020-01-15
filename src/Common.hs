module Common where

-- | The status of a build and a step, doc gives a few indications over the ordering's choice
data Status =
    PASS -- ^ Build or step succeeded
  | WARN -- ^ Build or step yielded a warning
  | CANCELLED -- ^ Build or step was cancelled, supersedes warnings
  | FAIL -- ^ Build or step failed, supersedes cancellation
  | ERROR -- ^ Internal zzbot error, supersedes everything
  deriving (Eq, Ord)

dbFile = "state.sqlite"

type BuilderID = Int
type BuildID = Int