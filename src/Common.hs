module Common where

-- | The status of a build and a step
data Status =
    PASS -- ^ Build or step succeeded
  | FAIL -- ^ Build or step failed
  | WARN -- ^ Build or step yielded a warning
  | ERROR -- ^ Internal zzbot error
  | CANCELLED -- ^ Build or step was cancelled