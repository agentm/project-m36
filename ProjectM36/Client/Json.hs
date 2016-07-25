{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Client.Json where
import Data.Aeson
import ProjectM36.Server.RemoteCallTypes.Json ()
import ProjectM36.Client

instance ToJSON EvaluatedNotification
instance FromJSON EvaluatedNotification