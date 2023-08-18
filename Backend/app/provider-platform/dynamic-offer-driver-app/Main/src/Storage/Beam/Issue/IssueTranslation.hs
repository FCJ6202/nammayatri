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

module Storage.Beam.Issue.IssueTranslation where

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

data IssueTranslationT f = IssueTranslationT
  { id :: B.C f Text,
    sentence :: B.C f Text,
    translation :: B.C f Text,
    language :: B.C f Language
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueTranslationT where
  data PrimaryKey IssueTranslationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type IssueTranslation = IssueTranslationT Identity

issueTranslationTMod :: IssueTranslationT (B.FieldModification (B.TableField IssueTranslationT))
issueTranslationTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      sentence = B.fieldNamed "sentence",
      translation = B.fieldNamed "translation",
      language = B.fieldNamed "language"
    }

$(enableKVPG ''IssueTranslationT ['id] [['language]])

$(mkTableInstances ''IssueTranslationT "issue_translation" "atlas_driver_offer_bpp")
