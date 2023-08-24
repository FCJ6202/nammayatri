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

module Storage.Beam.DriverBlockReason where

-- import qualified Dashboard.ProviderPlatform.Driver as Domain
-- import Data.ByteString.Internal (ByteString, unpackChars)
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
-- import Kernel.Types.Common hiding (id)
-- import Lib.Utils
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data DriverBlockReasonT f = DriverBlockReasonT
  { reasonCode :: B.C f Text,
    blockReason :: B.C f (Maybe Text),
    blockTimeInHours :: B.C f (Maybe Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverBlockReasonT where
  data PrimaryKey DriverBlockReasonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . reasonCode

type DriverBlockReason = DriverBlockReasonT Identity

driverBlockReasonTMod :: DriverBlockReasonT (B.FieldModification (B.TableField DriverBlockReasonT))
driverBlockReasonTMod =
  B.tableModification
    { reasonCode = B.fieldNamed "reason_code",
      blockReason = B.fieldNamed "block_reason",
      blockTimeInHours = B.fieldNamed "block_time_in_hours"
    }

$(enableKVPG ''DriverBlockReasonT ['reasonCode] [])

$(mkTableInstances ''DriverBlockReasonT "driver_block_reason" "atlas_driver_offer_bpp")
