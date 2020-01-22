module Common where

-- | The status of a build and a step, doc gives a few indications over the ordering's choice
data Status =
    Success -- ^ Build or step succeeded
  | Warning -- ^ Build or step yielded a warning
  | Cancelation -- ^ Build or step was cancelled, supersedes warnings
  | Failure -- ^ Build or step failed, supersedes cancellation
  | Error -- ^ Internal zzbot error, supersedes everything
  deriving (Eq, Ord, Show)

type BuildID = Int
