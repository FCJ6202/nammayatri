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

module Storage.Beam.FarePolicy.DriverExtraFeeBounds where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Vehicle.Variant as Vehicle
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize as Se

instance IsString Vehicle.Variant where
  fromString = show

data DriverExtraFeeBoundsT f = DriverExtraFeeBoundsT
  { id :: B.C f (Maybe Int),
    farePolicyId :: B.C f Text,
    startDistance :: B.C f Meters,
    minFee :: B.C f Money,
    maxFee :: B.C f Money
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverExtraFeeBoundsT where
  data PrimaryKey DriverExtraFeeBoundsT f
    = Id (B.C f (Maybe Int))
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverExtraFeeBounds = DriverExtraFeeBoundsT Identity

driverExtraFeeBoundsTMod :: DriverExtraFeeBoundsT (B.FieldModification (B.TableField DriverExtraFeeBoundsT))
driverExtraFeeBoundsTMod =
  B.tableModification
    { -- id = B.fieldNamed "id",
      farePolicyId = B.fieldNamed "fare_policy_id",
      startDistance = B.fieldNamed "start_distance",
      minFee = B.fieldNamed "min_fee",
      maxFee = B.fieldNamed "max_fee"
    }

$(enableKVPG ''DriverExtraFeeBoundsT ['id] [['farePolicyId]])

$(mkTableInstances ''DriverExtraFeeBoundsT "fare_policy_driver_extra_fee_bounds" "atlas_driver_offer_bpp")
