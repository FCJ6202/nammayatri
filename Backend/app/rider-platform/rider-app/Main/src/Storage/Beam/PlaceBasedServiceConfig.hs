{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PlaceBasedServiceConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketPlace
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data PlaceBasedServiceConfigT f = PlaceBasedServiceConfigT
  { merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    placeId :: B.C f Kernel.Prelude.Text,
    configValue :: B.C f Data.Aeson.Value,
    serviceName :: B.C f Domain.Types.Merchant.MerchantServiceConfig.ServiceName,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PlaceBasedServiceConfigT where
  data PrimaryKey PlaceBasedServiceConfigT f = PlaceBasedServiceConfigId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = PlaceBasedServiceConfigId . placeId

type PlaceBasedServiceConfig = PlaceBasedServiceConfigT Identity

$(enableKVPG ''PlaceBasedServiceConfigT ['placeId] [])

$(mkTableInstances ''PlaceBasedServiceConfigT "place_based_service_config")
