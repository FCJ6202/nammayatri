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

module Storage.Beam.Issue.IssueOption where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data IssueOptionT f = IssueOptionT
  { id :: B.C f Text,
    issueCategoryId :: B.C f Text,
    option :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueOptionT where
  data PrimaryKey IssueOptionT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type IssueOption = IssueOptionT Identity

issueOptionTMod :: IssueOptionT (B.FieldModification (B.TableField IssueOptionT))
issueOptionTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      issueCategoryId = B.fieldNamed "issue_category_id",
      option = B.fieldNamed "option"
    }

$(enableKVPG ''IssueOptionT ['id] [['issueCategoryId]])

$(mkTableInstances ''IssueOptionT "issue_option" "atlas_driver_offer_bpp")
