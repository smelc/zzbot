module Db where

import Common

data StepState = StepState String String Status -- ^ stdout, stderr, status
data BuildState = BuildState BuilderID [StepState]

class DbOperations m where
   startBuild :: String -> m BuildState -- ^ The string is the builder's name
   addStep :: m BuildState -> String -> String -> Status -> m BuildState -- ^ stdout, stderr, step status
   endBuild :: m Status -- ^ Returns the overall state of the build