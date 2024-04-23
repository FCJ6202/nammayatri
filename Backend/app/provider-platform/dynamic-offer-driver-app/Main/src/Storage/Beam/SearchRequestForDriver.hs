{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.SearchRequestForDriver where

import qualified Database.Beam as B
import qualified Domain.Types.DriverInformation as D
import qualified Domain.Types.SearchRequestForDriver as Domain
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.Vehicle as Variant
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data SearchRequestForDriverT f = SearchRequestForDriverT
  { id :: B.C f Text,
    requestId :: B.C f Text,
    searchTryId :: B.C f Text,
    merchantId :: B.C f (Maybe Text),
    merchantOperatingCityId :: B.C f (Maybe Text),
    startTime :: B.C f UTCTime,
    actualDistanceToPickup :: B.C f Meters,
    straightLineDistanceToPickup :: B.C f Meters,
    durationToPickup :: B.C f Seconds,
    vehicleVariant :: B.C f Variant.Variant,
    vehicleServiceTier :: B.C f (Maybe DVST.ServiceTierType),
    vehicleServiceTierName :: B.C f (Maybe Text),
    airConditioned :: B.C f (Maybe Bool),
    batchNumber :: B.C f Int,
    lat :: B.C f (Maybe Double),
    lon :: B.C f (Maybe Double),
    searchRequestValidTill :: B.C f LocalTime,
    driverId :: B.C f Text,
    status :: B.C f Domain.DriverSearchRequestStatus,
    response :: B.C f (Maybe Domain.SearchRequestForDriverResponse),
    driverMinExtraFee :: B.C f (Maybe Money),
    driverMaxExtraFee :: B.C f (Maybe Money),
    rideRequestPopupDelayDuration :: B.C f Seconds,
    isPartOfIntelligentPool :: B.C f Bool,
    pickupZone :: B.C f Bool,
    cancellationRatio :: B.C f (Maybe Double),
    acceptanceRatio :: B.C f (Maybe Double),
    driverAvailableTime :: B.C f (Maybe Double),
    parallelSearchRequestCount :: B.C f (Maybe Int),
    driverSpeed :: B.C f (Maybe Double),
    keepHiddenForSeconds :: B.C f Seconds,
    mode :: B.C f (Maybe D.DriverMode),
    goHomeRequestId :: B.C f (Maybe Text),
    rideFrequencyScore :: B.C f (Maybe Double),
    customerCancellationDues :: B.C f (Maybe HighPrecMoney),
    createdAt :: B.C f LocalTime,
    clientSdkVersion :: B.C f (Maybe Text),
    clientBundleVersion :: B.C f (Maybe Text),
    clientConfigVersion :: B.C f (Maybe Text),
    clientOsType :: B.C f (Maybe DeviceType),
    clientOsVersion :: B.C f (Maybe Text),
    backendConfigVersion :: B.C f (Maybe Text),
    backendAppVersion :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestForDriverT where
  data PrimaryKey SearchRequestForDriverT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SearchRequestForDriver = SearchRequestForDriverT Identity

$(enableKVPG ''SearchRequestForDriverT ['id] [['searchTryId], ['requestId]])

$(mkTableInstancesWithTModifier ''SearchRequestForDriverT "search_request_for_driver" [("requestId", "search_request_id")])
