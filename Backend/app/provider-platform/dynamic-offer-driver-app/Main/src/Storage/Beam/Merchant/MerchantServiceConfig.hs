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

module Storage.Beam.Merchant.MerchantServiceConfig where

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
import qualified Domain.Types.Merchant.MerchantServiceConfig as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import qualified Kernel.External.Call as Call
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.External.SMS.Interface as Sms
import qualified Kernel.External.Verification.Interface as Verification
import qualified Kernel.External.Whatsapp.Interface as Whatsapp
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Kernel.Utils.Common (decodeFromText, encodeToText)
import Kernel.Utils.Error
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Merchant (MerchantTId)
import Tools.Error

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

instance FromField Domain.ServiceName where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.ServiceName where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.ServiceName

instance FromBackendRow Postgres Domain.ServiceName

data MerchantServiceConfigT f = MerchantServiceConfigT
  { merchantId :: B.C f Text,
    serviceName :: B.C f Domain.ServiceName,
    configJSON :: B.C f Text,
    updatedAt :: B.C f Time.LocalTime,
    createdAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantServiceConfigT where
  data PrimaryKey MerchantServiceConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

instance ModelMeta MerchantServiceConfigT where
  modelFieldModification = merchantServiceConfigTMod
  modelTableName = "merchant_service_config"
  mkExprWithDefault _ = B.insertExpressions []

type MerchantServiceConfig = MerchantServiceConfigT Identity

instance FromJSON MerchantServiceConfig where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON MerchantServiceConfig where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show MerchantServiceConfig

merchantServiceConfigTMod :: MerchantServiceConfigT (B.FieldModification (B.TableField MerchantServiceConfigT))
merchantServiceConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      serviceName = B.fieldNamed "service_name",
      configJSON = B.fieldNamed "config_j_s_o_n",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

merchantServiceConfigToHSModifiers :: M.Map Text (A.Value -> A.Value)
merchantServiceConfigToHSModifiers =
  M.fromList
    []

merchantServiceConfigToPSModifiers :: M.Map Text (A.Value -> A.Value)
merchantServiceConfigToPSModifiers =
  M.fromList
    []

$(enableKVPG ''MerchantServiceConfigT ['serviceName] [])
