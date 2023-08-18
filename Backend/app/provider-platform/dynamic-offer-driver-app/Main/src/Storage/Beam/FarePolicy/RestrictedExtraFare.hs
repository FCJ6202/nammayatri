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

module Storage.Beam.FarePolicy.RestrictedExtraFare where

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
import Sequelize

instance IsString Vehicle.Variant where
  fromString = show

data RestrictedExtraFareT f = RestrictedExtraFareT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    vehicleVariant :: B.C f Vehicle.Variant,
    minTripDistance :: B.C f Meters,
    driverMaxExtraFare :: B.C f Money
  }
  deriving (Generic, B.Beamable)

instance B.Table RestrictedExtraFareT where
  data PrimaryKey RestrictedExtraFareT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type RestrictedExtraFare = RestrictedExtraFareT Identity

restrictedExtraFareTMod :: RestrictedExtraFareT (B.FieldModification (B.TableField RestrictedExtraFareT))
restrictedExtraFareTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      minTripDistance = B.fieldNamed "min_trip_distance",
      driverMaxExtraFare = B.fieldNamed "driver_max_extra_fare"
    }

$(enableKVPG ''RestrictedExtraFareT ['id] [])

$(mkTableInstances ''RestrictedExtraFareT "restricted_extra_fare" "atlas_driver_offer_bpp")
