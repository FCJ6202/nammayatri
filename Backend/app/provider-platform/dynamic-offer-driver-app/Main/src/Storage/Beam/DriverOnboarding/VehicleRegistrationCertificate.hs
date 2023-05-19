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

module Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate where

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
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Encryption
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.DriverOnboarding.Image (ImageTId)

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

instance FromField Domain.VerificationStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.VerificationStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.VerificationStatus

instance FromBackendRow Postgres Domain.VerificationStatus

instance FromField DbHash where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DbHash where
  sqlValueSyntax = autoSqlValueSyntax

instance HasSqlValueSyntax be String => HasSqlValueSyntax be [Text] where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DbHash

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Text]

instance FromBackendRow Postgres DbHash

data VehicleRegistrationCertificateT f = VehicleRegistrationCertificateT
  { id :: B.C f Text,
    documentImageId :: B.C f Text,
    certificateNumberEncrypted :: B.C f Text,
    certificateNumberHash :: B.C f DbHash,
    fitnessExpiry :: B.C f Time.UTCTime,
    permitExpiry :: B.C f (Maybe Time.UTCTime),
    pucExpiry :: B.C f (Maybe Time.UTCTime),
    insuranceValidity :: B.C f (Maybe Time.UTCTime),
    vehicleClass :: B.C f (Maybe Text),
    vehicleManufacturer :: B.C f (Maybe Text),
    vehicleCapacity :: B.C f (Maybe Int),
    vehicleModel :: B.C f (Maybe Text),
    vehicleColor :: B.C f (Maybe Text),
    vehicleEnergyType :: B.C f (Maybe Text),
    verificationStatus :: B.C f Domain.VerificationStatus,
    failedRules :: B.C f [Text],
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleRegistrationCertificateT where
  data PrimaryKey VehicleRegistrationCertificateT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta VehicleRegistrationCertificateT where
  modelFieldModification = vehicleRegistrationCertificateTMod
  modelTableName = "vehicle_registration_certificate"
  mkExprWithDefault _ = B.insertExpressions []

type VehicleRegistrationCertificate = VehicleRegistrationCertificateT Identity

instance FromJSON VehicleRegistrationCertificate where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON VehicleRegistrationCertificate where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show VehicleRegistrationCertificate

deriving stock instance Ord Domain.VerificationStatus

vehicleRegistrationCertificateTMod :: VehicleRegistrationCertificateT (B.FieldModification (B.TableField VehicleRegistrationCertificateT))
vehicleRegistrationCertificateTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      documentImageId = B.fieldNamed "document_image_id",
      certificateNumberEncrypted = B.fieldNamed "certificate_number_encrypted",
      certificateNumberHash = B.fieldNamed "certificate_number_hash",
      fitnessExpiry = B.fieldNamed "fitness_expiry",
      permitExpiry = B.fieldNamed "permit_expiry",
      pucExpiry = B.fieldNamed "puc_expiry",
      insuranceValidity = B.fieldNamed "insurance_validity",
      vehicleClass = B.fieldNamed "vehicle_class",
      vehicleManufacturer = B.fieldNamed "vehicle_manufacturer",
      vehicleCapacity = B.fieldNamed "vehicle_capacity",
      vehicleModel = B.fieldNamed "vehicle_model",
      vehicleColor = B.fieldNamed "vehicle_color",
      vehicleEnergyType = B.fieldNamed "vehicle_energy_type",
      verificationStatus = B.fieldNamed "verification_status",
      failedRules = B.fieldNamed "failed_rules",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

vehicleRegistrationCertificateToHSModifiers :: M.Map Text (A.Value -> A.Value)
vehicleRegistrationCertificateToHSModifiers =
  M.fromList
    []

vehicleRegistrationCertificateToPSModifiers :: M.Map Text (A.Value -> A.Value)
vehicleRegistrationCertificateToPSModifiers =
  M.fromList
    []

instance IsString DbHash where
  fromString = show

instance IsString Domain.VerificationStatus where
  fromString = show

instance Serialize VehicleRegistrationCertificate where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''VehicleRegistrationCertificateT ['id] [])
