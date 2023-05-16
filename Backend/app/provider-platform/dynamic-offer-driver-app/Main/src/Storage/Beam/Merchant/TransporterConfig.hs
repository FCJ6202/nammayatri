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
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Merchant.TransporterConfig where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.Merchant.TransporterConfig as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import qualified Kernel.External.FCM.Types as FCM
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common
import Kernel.Types.Common hiding (id)
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Merchant (MerchantTId)

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case (readMaybe (unpackChars value')) of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

instance FromField Meters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

instance FromField Seconds where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Seconds where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

instance FromBackendRow Postgres Seconds

instance FromField Centesimal where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centesimal where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

instance FromBackendRow Postgres Centesimal

data TransporterConfigT f = TransporterConfigT
  { merchantId :: B.C f Text,
    pickupLocThreshold :: B.C f Meters,
    dropLocThreshold :: B.C f Meters,
    rideTimeEstimatedThreshold :: B.C f Seconds,
    includeDriverCurrentlyOnRide :: B.C f Bool,
    defaultPopupDelay :: B.C f Seconds,
    popupDelayToAddAsPenalty :: B.C f (Maybe Seconds),
    thresholdCancellationScore :: B.C f (Maybe Int),
    minRidesForCancellationScore :: B.C f (Maybe Int),
    mediaFileUrlPattern :: B.C f Text,
    mediaFileSizeUpperLimit :: B.C f Int,
    waitingTimeEstimatedThreshold :: B.C f Seconds,
    referralLinkPassword :: B.C f Text,
    fcmUrl :: B.C f Text,
    fcmServiceAccount :: B.C f Text,
    fcmTokenKeyPrefix :: B.C f Text,
    onboardingTryLimit :: B.C f Int,
    onboardingRetryTimeInHours :: B.C f Int,
    checkImageExtractionForDashboard :: B.C f Bool,
    searchRepeatLimit :: B.C f Int,
    actualRideDistanceDiffThreshold :: B.C f Centesimal,
    upwardsRecomputeBuffer :: B.C f Centesimal,
    approxRideDistanceDiffThreshold :: B.C f Centesimal,
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TransporterConfigT where
  data PrimaryKey TransporterConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

instance ModelMeta TransporterConfigT where
  modelFieldModification = transporterConfigTMod
  modelTableName = "transporter_config"
  mkExprWithDefault _ = B.insertExpressions []

type TransporterConfig = TransporterConfigT Identity

instance FromJSON TransporterConfig where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON TransporterConfig where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show TransporterConfig

transporterConfigTMod :: TransporterConfigT (B.FieldModification (B.TableField TransporterConfigT))
transporterConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      pickupLocThreshold = B.fieldNamed "pickup_loc_threshold",
      dropLocThreshold = B.fieldNamed "drop_loc_threshold",
      rideTimeEstimatedThreshold = B.fieldNamed "ride_time_estimated_threshold",
      includeDriverCurrentlyOnRide = B.fieldNamed "include_driver_currently_on_ride",
      defaultPopupDelay = B.fieldNamed "default_popup_delay",
      popupDelayToAddAsPenalty = B.fieldNamed "popup_delay_to_add_as_penalty",
      thresholdCancellationScore = B.fieldNamed "threshold_cancellation_score",
      minRidesForCancellationScore = B.fieldNamed "min_rides_for_cancellation_score",
      mediaFileUrlPattern = B.fieldNamed "media_file_url_pattern",
      mediaFileSizeUpperLimit = B.fieldNamed "media_file_size_upper_limit",
      waitingTimeEstimatedThreshold = B.fieldNamed "waiting_time_estimated_threshold",
      referralLinkPassword = B.fieldNamed "referral_link_password",
      fcmUrl = B.fieldNamed "fcm_url",
      fcmServiceAccount = B.fieldNamed "fcm_service_account",
      fcmTokenKeyPrefix = B.fieldNamed "fcm_token_key_prefix",
      onboardingTryLimit = B.fieldNamed "onboarding_try_limit",
      onboardingRetryTimeInHours = B.fieldNamed "onboarding_retry_time_in_hours",
      checkImageExtractionForDashboard = B.fieldNamed "check_image_extraction_for_dashboard",
      searchRepeatLimit = B.fieldNamed "search_repeat_limit",
      actualRideDistanceDiffThreshold = B.fieldNamed "actual_ride_distance_diff_threshold",
      upwardsRecomputeBuffer = B.fieldNamed "upwards_recompute_buffer",
      approxRideDistanceDiffThreshold = B.fieldNamed "approx_ride_distance_diff_threshold",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

transporterConfigToHSModifiers :: M.Map Text (A.Value -> A.Value)
transporterConfigToHSModifiers =
  M.fromList
    []

transporterConfigToPSModifiers :: M.Map Text (A.Value -> A.Value)
transporterConfigToPSModifiers =
  M.fromList
    []

$(enableKVPG ''TransporterConfigT ['merchantId] [])
