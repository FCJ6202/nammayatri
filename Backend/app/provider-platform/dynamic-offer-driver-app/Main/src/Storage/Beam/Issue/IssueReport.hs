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

module Storage.Beam.Issue.IssueReport where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Issue.IssueReport as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Domain.IssueStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.IssueStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.IssueStatus

instance FromBackendRow Postgres Domain.IssueStatus

instance IsString Domain.IssueStatus where
  fromString = show

data IssueReportT f = IssueReportT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    rideId :: B.C f (Maybe Text),
    description :: B.C f Text,
    assignee :: B.C f (Maybe Text),
    status :: B.C f Domain.IssueStatus,
    categoryId :: B.C f Text,
    optionId :: B.C f (Maybe Text),
    deleted :: B.C f Bool,
    mediaFiles :: B.C f [Text],
    ticketId :: B.C f (Maybe Text),
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueReportT where
  data PrimaryKey IssueReportT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type IssueReport = IssueReportT Identity

issueReportTMod :: IssueReportT (B.FieldModification (B.TableField IssueReportT))
issueReportTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      rideId = B.fieldNamed "ride_id",
      description = B.fieldNamed "description",
      assignee = B.fieldNamed "assignee",
      status = B.fieldNamed "status",
      categoryId = B.fieldNamed "category_id",
      optionId = B.fieldNamed "option_id",
      deleted = B.fieldNamed "deleted",
      mediaFiles = B.fieldNamed "media_files",
      ticketId = B.fieldNamed "ticket_id",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''IssueReportT ['id] [['driverId], ['categoryId], ['ticketId]])
$(mkTableInstances ''IssueReportT "issue_report" "atlas_driver_offer_bpp")
