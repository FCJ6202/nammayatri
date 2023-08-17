{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.MerchantConfig where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString)
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Lib.Utils ()
import Sequelize

fromFieldSWC ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion SWC.SlidingWindowOptions
fromFieldSWC f mbValue = do
  value <- fromField f mbValue
  case A.fromJSON value of
    A.Success a -> pure a
    _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance IsString Minutes where
  fromString = show

instance FromField SWC.SlidingWindowOptions where
  fromField = fromFieldSWC

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be SWC.SlidingWindowOptions where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be SWC.SlidingWindowOptions

instance FromBackendRow Postgres SWC.SlidingWindowOptions

instance IsString SWC.SlidingWindowOptions where
  fromString = show

deriving stock instance Ord SWC.PeriodType

deriving stock instance Ord SWC.SlidingWindowOptions

deriving stock instance Eq SWC.SlidingWindowOptions

data MerchantConfigT f = MerchantConfigT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    fraudBookingCancellationCountThreshold :: B.C f Int,
    fraudBookingCancellationCountWindow :: B.C f SWC.SlidingWindowOptions,
    fraudBookingTotalCountThreshold :: B.C f Int,
    fraudBookingCancelledByDriverCountThreshold :: B.C f Int,
    fraudBookingCancelledByDriverCountWindow :: B.C f SWC.SlidingWindowOptions,
    fraudSearchCountThreshold :: B.C f Int,
    fraudSearchCountWindow :: B.C f SWC.SlidingWindowOptions,
    fraudRideCountThreshold :: B.C f Int,
    fraudRideCountWindow :: B.C f SWC.SlidingWindowOptions,
    enabled :: B.C f Bool
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantConfigT where
  data PrimaryKey MerchantConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MerchantConfig = MerchantConfigT Identity

merchantConfigTMod :: MerchantConfigT (B.FieldModification (B.TableField MerchantConfigT))
merchantConfigTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      fraudBookingCancellationCountThreshold = B.fieldNamed "fraud_booking_cancellation_count_threshold",
      fraudBookingCancellationCountWindow = B.fieldNamed "fraud_booking_cancellation_count_window",
      fraudBookingTotalCountThreshold = B.fieldNamed "fraud_booking_total_count_threshold",
      fraudBookingCancelledByDriverCountThreshold = B.fieldNamed "fraud_booking_cancelled_by_driver_count_threshold",
      fraudBookingCancelledByDriverCountWindow = B.fieldNamed "fraud_booking_cancelled_by_driver_count_window",
      fraudSearchCountThreshold = B.fieldNamed "fraud_search_count_threshold",
      fraudSearchCountWindow = B.fieldNamed "fraud_search_count_window",
      fraudRideCountThreshold = B.fieldNamed "fraud_ride_count_threshold",
      fraudRideCountWindow = B.fieldNamed "fraud_ride_count_window",
      enabled = B.fieldNamed "enabled"
    }

$(enableKVPG ''MerchantConfigT ['id] [['merchantId]])

$(mkTableInstances ''MerchantConfigT "merchant_config" "atlas_app")
