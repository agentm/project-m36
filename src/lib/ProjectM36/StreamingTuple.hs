{-# LANGUAGE FlexibleContexts #-}
module ProjectM36.StreamingTuple where
import ProjectM36.Base
import ProjectM36.Error
import Streamly
import Control.Monad.Catch
import qualified Streamly.Prelude as S
import ProjectM36.Tuple
import qualified Data.Set as Set

type RestrictionFilter = RelationTuple -> Either RelationalError Bool

restrict :: MonadAsync m => RestrictionFilter -> StreamT m RelationTuple -> StreamT m RelationTuple
restrict func s = s >>= \tupIn -> case func tupIn of
  Left err -> throwM err
  Right filt -> if filt then pure tupIn else S.nil
  
cardinality :: MonadAsync m => StreamT m RelationTuple -> m RelationCardinality
cardinality m = Finite <$> S.length m

-- error handling needs to happen at type-checking time
rename :: MonadAsync m => AttributeName -> AttributeName -> StreamT m RelationTuple -> StreamT m RelationTuple
rename attrA attrB s = S.mapM (\tup -> pure (tupleRenameAttribute attrA attrB tup)) s

project :: MonadAsync m => Set.Set AttributeName -> StreamT m RelationTuple -> StreamT m RelationTuple
project attrs s = S.mapM (\tup -> pure (tupleProject attrs tup)) s

union :: MonadAsync m => StreamT m RelationTuple -> StreamT m RelationTuple -> StreamT m RelationTuple
union = (<>)

--nested loop join requires traversing one of the stream n times
join :: MonadAsync m => StreamT m RelationTuple -> StreamT m RelationTuple -> StreamT m RelationTuple
join s1 s2 = do
  