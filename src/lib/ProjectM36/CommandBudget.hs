-- | The user provides a "command budget" for each request which the backend can use to prioritize or alter the execution plan to accommodate 
module ProjectM36.CommandBudget where

data CommandBudget =
  UserInteractiveTarget | -- the user is waiting to view the result and he needs it ASAP, choose the fastest plan
  ApproximateTarget Int | -- 0-100 the user is willing to sacrifice accuracy for speed of result, use approximations for aggregates, for example, but should this just be a different query or can this modify and existing query which demands 100% accuracy?
  BatchTarget | -- the command is part of a background/batch job which is typically the lowest priority task
  CPUHeavyTarget? | -- lean into the CPU whenever possible to reduce IO
  IOHeavyTarget? | -- lean into IO whenever possible to reduce CPU usage
  FutureTimeTarget UTCTime DeadlineAction | -- the deadline includes network round trip and db planning and db execution times so that database users know when commands take too long under any circumstance, not just when db performance is below expectations, the db can track all these times and report on them


data DeadlineAction =
  CancelIfBeyondDeadlineAction | -- a hard deadline which cancels the command if executing the command has extended beyond the deadline- the result is no longer useful
  NoAction | -- a soft deadline which continues running the command, but note/track/log that the command exceeded the expected deadline- can be used for knowing when queries are extending beyond their expected performance deadlines
