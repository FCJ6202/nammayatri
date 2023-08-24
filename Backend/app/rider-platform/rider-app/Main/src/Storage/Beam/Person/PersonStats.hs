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

module Storage.Beam.Person.PersonStats where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data PersonStatsT f = PersonStatsT
  { personId :: B.C f Text,
    userCancelledRides :: B.C f Int,
    driverCancelledRides :: B.C f Int,
    completedRides :: B.C f Int,
    weekendRides :: B.C f Int,
    weekdayRides :: B.C f Int,
    offPeakRides :: B.C f Int,
    eveningPeakRides :: B.C f Int,
    morningPeakRides :: B.C f Int,
    weekendPeakRides :: B.C f Int,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonStatsT where
  data PrimaryKey PersonStatsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . personId

type PersonStats = PersonStatsT Identity

personStatsTMod :: PersonStatsT (B.FieldModification (B.TableField PersonStatsT))
personStatsTMod =
  B.tableModification
    { personId = B.fieldNamed "person_id",
      userCancelledRides = B.fieldNamed "user_cancelled_rides",
      driverCancelledRides = B.fieldNamed "driver_cancelled_rides",
      completedRides = B.fieldNamed "completed_rides",
      weekendRides = B.fieldNamed "weekend_rides",
      weekdayRides = B.fieldNamed "weekday_rides",
      offPeakRides = B.fieldNamed "off_peak_rides",
      eveningPeakRides = B.fieldNamed "evening_peak_rides",
      morningPeakRides = B.fieldNamed "morning_peak_rides",
      weekendPeakRides = B.fieldNamed "weekend_peak_rides",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''PersonStatsT ['personId] [])
$(mkTableInstances ''PersonStatsT "person_stats" "atlas_app")
