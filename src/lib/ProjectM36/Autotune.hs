-- | Run some quick tests at database startup to determine appropriate tuning configuration parameters.
module ProjectM36.Autotune where
import ProjectM36.Base

data Tunables = Tunables {
--  tupleBatch :: Integer -- ^ the number of tuples which to send to parallelization units (streamly
  tupleBatchRead :: Integer, -- ^ the estimated size of a set of tuples to read from disk before parallelization makes sense
  tupleBatchWrite :: Integer -- ^ the estimated size of a set of tuples to write to disk before parallelization makes sense
  }


--generate 10000 tuples and adjust parameters to find sweet spot

--find optimal size of tuple writes and reads separately- record both

mkTuningTupleSet :: Int -> [RelationTuple]
mkTuningTupleSet ntups =
  map mkTup [0..ntups]
  where
    epoch = posixSecondsToUTCTime 0
    days x = secondsToNominalDiffTime (24 * 60 * 60 * x)
    attrs = attributesFromList [Attribute "a" IntegerAtomType,
                                Attribute "b" TextAtomType,
                                Attribute "c" DateTimeAtomType,
                                Attribute "d" TextAtomType,
                                Attribute "e" IntegerAtomType]
    mkTup i = RelationTuple attrs (V.fromList [IntegerAtom i,
                                               TextAtom (T.pack (show i)),
                                               DateTimeAtom (addUTCTime (days i) epoch),
                                               TextAtom ("tupleString_" <> T.pack (show i)),
                                               IntegerAtom (i + 10)])
  
calcTupleBatchWrite :: FilePath -> IO Integer
calcTupleBatchWrite targetDir = do
  --create tupleset of x tuples of 5 attributes
  --generate exponential sizes but with logarithmic backoff
  let expUp = map (5 ^) [5..]
  writeTupChunks <- foldM writeFolder 1000 expUp 
  readTupChunks   
  --once range is 1000 tuples, consider it stable and return the value
