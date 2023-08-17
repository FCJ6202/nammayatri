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

module Storage.Beam.Driver.DriverFlowStatus where

import Data.Aeson
import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString)
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Driver.DriverFlowStatus as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

fromFieldFlowStatus ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Domain.FlowStatus
fromFieldFlowStatus f mbValue = do
  value <- fromField f mbValue
  case A.fromJSON value of
    A.Success a -> pure a
    _ -> DPSF.returnError ConversionFailed f "Conversion failed"

instance FromField Domain.FlowStatus where
  fromField = fromFieldFlowStatus

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be Domain.FlowStatus where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.FlowStatus

instance FromBackendRow Postgres Domain.FlowStatus

instance IsString Domain.FlowStatus where
  fromString = show

data DriverFlowStatusT f = DriverFlowStatusT
  { personId :: B.C f Text,
    flowStatus :: B.C f Domain.FlowStatus,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverFlowStatusT where
  data PrimaryKey DriverFlowStatusT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . personId

type DriverFlowStatus = DriverFlowStatusT Identity

deriving stock instance Ord Domain.FlowStatus

deriving stock instance Read Domain.FlowStatus

driverFlowStatusTMod :: DriverFlowStatusT (B.FieldModification (B.TableField DriverFlowStatusT))
driverFlowStatusTMod =
  B.tableModification
    { personId = B.fieldNamed "person_id",
      flowStatus = B.fieldNamed "flow_status",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''DriverFlowStatusT ['personId] [])

$(mkTableInstances ''DriverFlowStatusT "driver_flow_status" "atlas_driver_offer_bpp")
