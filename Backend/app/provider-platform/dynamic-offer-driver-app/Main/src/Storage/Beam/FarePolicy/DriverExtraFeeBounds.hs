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

module Storage.Beam.FarePolicy.DriverExtraFeeBounds where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
-- import Database.Beam.Backend

-- import Database.Beam.Postgres
--   ( Postgres,
--   )
-- import Database.PostgreSQL.Simple.FromField (FromField, fromField)
-- -- import qualified Domain.Types.FarePolicy.DriverExtraFeeBounds as Domain

-- import qualified Domain.Types.FareParameters as Domain

-- import Lib.Utils

-- import Database.Beam.Backend
import Database.Beam.MySQL ()
-- import Database.Beam.Postgres
-- import Database.PostgreSQL.Simple.FromField (FromField, fromField)
-- import qualified Domain.Types.FarePolicy
-- import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Vehicle
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize as Se
import Storage.Tabular.Vehicle ()

instance IsString Vehicle.Variant where
  fromString = show

instance IsString Meters where
  fromString = show

instance IsString Money where
  fromString = show

data DriverExtraFeeBoundsT f = DriverExtraFeeBoundsT
  { -- id :: B.C f Text,
    farePolicyId :: B.C f Text,
    startDistance :: B.C f Meters,
    minFee :: B.C f Money,
    maxFee :: B.C f Money
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverExtraFeeBoundsT where
  data PrimaryKey DriverExtraFeeBoundsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . farePolicyId

instance ModelMeta DriverExtraFeeBoundsT where
  modelFieldModification = driverExtraFeeBoundsTMod
  modelTableName = "fare_parameters_progressive_details"
  mkExprWithDefault _ = B.insertExpressions []

type DriverExtraFeeBounds = DriverExtraFeeBoundsT Identity

instance FromJSON DriverExtraFeeBounds where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverExtraFeeBounds where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverExtraFeeBounds

-- instance FromField Domain.NightShiftCharge where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.NightShiftCharge where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.NightShiftCharge

-- instance FromBackendRow Postgres Domain.NightShiftCharge

-- deriving stock instance Read Domain.NightShiftCharge

-- deriving stock instance Read Domain.WaitingCharge

driverExtraFeeBoundsTMod :: DriverExtraFeeBoundsT (B.FieldModification (B.TableField DriverExtraFeeBoundsT))
driverExtraFeeBoundsTMod =
  B.tableModification
    { -- id = B.fieldNamed "id",
      farePolicyId = B.fieldNamed "fare_policy_id",
      startDistance = B.fieldNamed "start_distance",
      minFee = B.fieldNamed "min_fee",
      maxFee = B.fieldNamed "max_fee"
    }

instance Serialize DriverExtraFeeBounds where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverExtraFeeBoundsToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverExtraFeeBoundsToHSModifiers =
  M.empty

driverExtraFeeBoundsToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverExtraFeeBoundsToPSModifiers =
  M.empty

$(enableKVPG ''DriverExtraFeeBoundsT ['farePolicyId] [])
