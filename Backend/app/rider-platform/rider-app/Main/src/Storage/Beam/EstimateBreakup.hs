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

module Storage.Beam.EstimateBreakup where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

data EstimateBreakupT f = EstimateBreakupT
  { id :: B.C f Text,
    estimateId :: B.C f Text,
    title :: B.C f Text,
    priceCurrency :: B.C f Text,
    priceValue :: B.C f HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table EstimateBreakupT where
  data PrimaryKey EstimateBreakupT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type EstimateBreakup = EstimateBreakupT Identity

estimateBreakupTMod :: EstimateBreakupT (B.FieldModification (B.TableField EstimateBreakupT))
estimateBreakupTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      estimateId = B.fieldNamed "estimate_id",
      title = B.fieldNamed "title",
      priceCurrency = B.fieldNamed "price_currency",
      priceValue = B.fieldNamed "price_value"
    }

$(enableKVPG ''EstimateBreakupT ['id] [['estimateId]])

$(mkTableInstances ''EstimateBreakupT "estimate_breakup" "atlas_app")
