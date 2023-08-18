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

module Storage.Beam.PlanTranslation where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Types (Language)
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize

instance IsString Language where
  fromString = show

data PlanTranslationT f = PlanTranslationT
  { planId :: B.C f Text,
    language :: B.C f Language,
    name :: B.C f Text,
    description :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table PlanTranslationT where
  data PrimaryKey PlanTranslationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . planId

type PlanTranslation = PlanTranslationT Identity

planTranslationTMod :: PlanTranslationT (B.FieldModification (B.TableField PlanTranslationT))
planTranslationTMod =
  B.tableModification
    { planId = B.fieldNamed "plan_id",
      language = B.fieldNamed "language",
      name = B.fieldNamed "name",
      description = B.fieldNamed "description"
    }

$(enableKVPG ''PlanTranslationT ['planId] [['language]])
$(mkTableInstances ''PlanTranslationT "plan_translation" "atlas_driver_offer_bpp")
