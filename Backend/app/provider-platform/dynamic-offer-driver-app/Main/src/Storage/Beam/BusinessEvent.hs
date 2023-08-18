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

module Storage.Beam.BusinessEvent where

import qualified Data.Aeson as A
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.BusinessEvent as Domain
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Domain.EventType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.EventType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.EventType

instance FromBackendRow Postgres Domain.EventType

instance FromField Domain.WhenPoolWasComputed where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.WhenPoolWasComputed where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.WhenPoolWasComputed

instance FromBackendRow Postgres Domain.WhenPoolWasComputed

data BusinessEventT f = BusinessEventT
  { id :: B.C f Text,
    driverId :: B.C f (Maybe Text),
    eventType :: B.C f Domain.EventType,
    timeStamp :: B.C f Time.UTCTime,
    bookingId :: B.C f (Maybe Text),
    whenPoolWasComputed :: B.C f (Maybe Domain.WhenPoolWasComputed),
    vehicleVariant :: B.C f (Maybe Variant),
    distance :: B.C f (Maybe Int),
    duration :: B.C f (Maybe Int),
    rideId :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table BusinessEventT where
  data PrimaryKey BusinessEventT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type BusinessEvent = BusinessEventT Identity

instance FromJSON Domain.EventType where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Domain.EventType where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON Domain.WhenPoolWasComputed where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Domain.WhenPoolWasComputed where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Ord Domain.EventType

deriving stock instance Ord Domain.WhenPoolWasComputed

instance IsString Domain.EventType where
  fromString = show

instance IsString Domain.WhenPoolWasComputed where
  fromString = show

businessEventTMod :: BusinessEventT (B.FieldModification (B.TableField BusinessEventT))
businessEventTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      eventType = B.fieldNamed "event_type",
      timeStamp = B.fieldNamed "time_stamp",
      bookingId = B.fieldNamed "booking_id",
      whenPoolWasComputed = B.fieldNamed "when_pool_was_computed",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      distance = B.fieldNamed "distance",
      duration = B.fieldNamed "duration",
      rideId = B.fieldNamed "ride_id"
    }

$(enableKVPG ''BusinessEventT ['id] [])

$(mkTableInstances ''BusinessEventT "business_event" "atlas_driver_offer_bpp")
