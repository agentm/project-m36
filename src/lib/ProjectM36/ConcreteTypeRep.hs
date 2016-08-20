{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
-- adapted from https://hackage.haskell.org/package/concrete-typerep
module ProjectM36.ConcreteTypeRep where
import Data.Binary
import Data.Typeable
import Data.Typeable.Internal
import Control.DeepSeq (NFData)
import GHC.Generics

newtype ConcreteTypeRep = CTR { unCTR :: TypeRep } deriving (Eq, Typeable, NFData)

newtype SerialRep = SR (TyConRep, [SerialRep]) deriving (Binary, Generic)

instance Show ConcreteTypeRep where
  show (CTR tRep) = show tRep

type TyConRep = (String, String, String)
toTyConRep :: TyCon -> TyConRep
toTyConRep (TyCon _ pack modul name) = (pack, modul, name)

fromTyConRep :: TyConRep -> TyCon
fromTyConRep (pack, modul, name) = mkTyCon3 pack modul name

toSerial :: ConcreteTypeRep -> SerialRep
toSerial (CTR t) = case splitTyConApp t of
  (con, args) -> SR (toTyConRep con, map (toSerial . CTR) args)
          
fromSerial :: SerialRep -> ConcreteTypeRep          
fromSerial (SR (con, args)) = CTR $ mkTyConApp (fromTyConRep con) (map (unCTR . fromSerial) args)

instance Binary ConcreteTypeRep where
    put = put . toSerial
    get = fromSerial <$> get
    
