{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.BookingCancellationReason where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.BookingCancellationReason as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Domain.CancellationSource where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.CancellationSource where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.CancellationSource

instance FromBackendRow Postgres Domain.CancellationSource

data BookingCancellationReasonT f = BookingCancellationReasonT
  { driverId :: B.C f (Maybe Text),
    bookingId :: B.C f Text,
    rideId :: B.C f (Maybe Text),
    merchantId :: B.C f (Maybe Text),
    source :: B.C f Domain.CancellationSource,
    reasonCode :: B.C f (Maybe Text),
    additionalInfo :: B.C f (Maybe Text),
    driverCancellationLocationLat :: B.C f (Maybe Double),
    driverCancellationLocationLon :: B.C f (Maybe Double),
    driverDistToPickup :: B.C f (Maybe Meters)
  }
  deriving (Generic, B.Beamable)

instance IsString Domain.CancellationSource where
  fromString = show

instance B.Table BookingCancellationReasonT where
  data PrimaryKey BookingCancellationReasonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . bookingId

type BookingCancellationReason = BookingCancellationReasonT Identity

bookingCancellationReasonTMod :: BookingCancellationReasonT (B.FieldModification (B.TableField BookingCancellationReasonT))
bookingCancellationReasonTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      bookingId = B.fieldNamed "booking_id",
      rideId = B.fieldNamed "ride_id",
      merchantId = B.fieldNamed "merchant_id",
      source = B.fieldNamed "source",
      reasonCode = B.fieldNamed "reason_code",
      additionalInfo = B.fieldNamed "additional_info",
      driverCancellationLocationLat = B.fieldNamed "driver_cancellation_location_lat",
      driverCancellationLocationLon = B.fieldNamed "driver_cancellation_location_lon",
      driverDistToPickup = B.fieldNamed "driver_dist_to_pickup"
    }

$(enableKVPG ''BookingCancellationReasonT ['bookingId] [['rideId]])

$(mkTableInstances ''BookingCancellationReasonT "booking_cancellation_reason" "atlas_driver_offer_bpp")
