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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Storage.Beam.DriverLocation where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import qualified Database.Beam.Schema.Tables as B
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Storage.Esqueleto (Point (..))
import Kernel.Types.Common hiding (id)
import Lib.Schema
import Lib.Utils ()
import Sequelize

fromFieldPoint ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Point
fromFieldPoint f mbValue = case mbValue of
  Nothing -> DPSF.returnError DPSF.UnexpectedNull f mempty
  Just _ -> pure Point

instance FromField Point where
  fromField = fromFieldPoint

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Point where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Point

instance FromBackendRow Postgres Point

deriving anyclass instance A.FromJSON Point

deriving stock instance Ord Point

deriving anyclass instance A.ToJSON Point

data DriverLocationT f = DriverLocationT
  { driverId :: B.C f Text,
    lat :: B.C f Double,
    lon :: B.C f Double,
    point :: B.C f Point,
    coordinatesCalculatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime,
    merchantId :: B.C f Text
  }
  deriving (Generic, B.Beamable)

driverLocationTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverLocationT)
driverLocationTable =
  B.setEntitySchema (Just getLocationSchemaName)
    <> B.setEntityName "driver_location"
    <> B.modifyTableFields driverLocationTMod

instance B.Table DriverLocationT where
  data PrimaryKey DriverLocationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

instance ModelMeta DriverLocationT where
  modelFieldModification = driverLocationTMod
  modelTableName = "driver_location"
  modelSchemaName = Just getLocationSchemaName

type DriverLocation = DriverLocationT Identity

instance FromJSON DriverLocation where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverLocation where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverLocation

instance IsString Point where
  fromString = show

driverLocationTMod :: DriverLocationT (B.FieldModification (B.TableField DriverLocationT))
driverLocationTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      lat = B.fieldNamed "lat",
      lon = B.fieldNamed "lon",
      point = B.fieldNamed "point",
      coordinatesCalculatedAt = B.fieldNamed "coordinates_calculated_at",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      merchantId = B.fieldNamed "merchant_id"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverLocationToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverLocationToHSModifiers =
  M.empty

driverLocationToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverLocationToPSModifiers =
  M.empty

toRowExpression personId latLong updateTime now merchantId =
  DriverLocationT
    (B.val_ personId)
    (B.val_ latLong.lat)
    (B.val_ latLong.lon)
    (getPoint (latLong.lat, latLong.lon))
    (B.val_ updateTime)
    (B.val_ now)
    (B.val_ now)
    (B.val_ merchantId)

instance Serialize DriverLocation where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DriverLocationT ['driverId] [])
