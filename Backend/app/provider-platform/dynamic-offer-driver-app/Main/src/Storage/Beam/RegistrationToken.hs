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

module Storage.Beam.RegistrationToken where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.RegistrationToken as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

data RegistrationTokenT f = RegistrationTokenT
  { id :: B.C f Text,
    token :: B.C f RegToken,
    attempts :: B.C f Int,
    authMedium :: B.C f Domain.Medium,
    authType :: B.C f Domain.LoginType,
    authValueHash :: B.C f Text,
    verified :: B.C f Bool,
    authExpiry :: B.C f Int,
    tokenExpiry :: B.C f Int,
    entityId :: B.C f Text,
    merchantId :: B.C f Text,
    entityType :: B.C f Domain.RTEntityType,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime,
    info :: B.C f (Maybe Text),
    alternateNumberAttempts :: B.C f Int
  }
  deriving (Generic, B.Beamable)

instance B.Table RegistrationTokenT where
  data PrimaryKey RegistrationTokenT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type RegistrationToken = RegistrationTokenT Identity

registrationTokenTMod :: RegistrationTokenT (B.FieldModification (B.TableField RegistrationTokenT))
registrationTokenTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      token = B.fieldNamed "token",
      attempts = B.fieldNamed "attempts",
      authMedium = B.fieldNamed "auth_medium",
      authType = B.fieldNamed "auth_type",
      authValueHash = B.fieldNamed "auth_value_hash",
      verified = B.fieldNamed "verified",
      authExpiry = B.fieldNamed "auth_expiry",
      tokenExpiry = B.fieldNamed "token_expiry",
      entityId = B.fieldNamed "entity_id",
      merchantId = B.fieldNamed "merchant_id",
      entityType = B.fieldNamed "entity_type",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      info = B.fieldNamed "info",
      alternateNumberAttempts = B.fieldNamed "alternate_number_attempts"
    }

$(enableKVPG ''RegistrationTokenT ['id] [['token], ['entityId]])

$(mkTableInstances ''RegistrationTokenT "registration_token" "atlas_driver_offer_bpp")
