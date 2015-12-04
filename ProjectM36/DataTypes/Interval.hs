{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.DataTypes.Interval where
import Data.Interval
import ProjectM36.Base hiding (Finite)
import Data.Binary 
import ProjectM36.Atom
import Data.Typeable
import Data.Time.Clock

instance (Atomable a, Ord a) => Atomable (Interval a)

instance (Binary a) => Binary (Extended a) where
  put ext = case ext of
    NegInf -> put (1 :: Int)
    PosInf -> put (2 :: Int)
    Finite x -> put (3 :: Int) >> put x
  get = do
    flag <- get :: Get Int
    case flag of
      1 -> pure NegInf
      2 -> pure PosInf
      3 -> Finite <$> get
      _ -> error "poop"

instance (Ord a, Binary a) => Binary (Interval a) where
  put interv = put (lowerBound interv) >> put (upperBound interv)
  get = interval <$> get <*> get
  
intervalFunctions :: [AtomFunction]
intervalFunctions = [AtomFunction { atomFuncName = "interval_dt_member",
                                    atomFuncType = [dateTimeAtomType,
                                                    atomTypeForProxy (Proxy :: Proxy (Interval UTCTime)), 
                                                    boolAtomType],
                                    atomFunc = (\(dt:dtinterval:_) -> Atom $ member ((unsafeCast dt)::UTCTime) ((unsafeCast dtinterval)::(Interval UTCTime)))}
  
  ]