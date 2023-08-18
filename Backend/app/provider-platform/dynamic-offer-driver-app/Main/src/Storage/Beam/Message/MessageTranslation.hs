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

module Storage.Beam.Message.MessageTranslation where

import qualified Data.Serialize
import qualified Data.Time as Time
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

data MessageTranslationT f = MessageTranslationT
  { messageId :: B.C f Text,
    language :: B.C f Language,
    title :: B.C f Text,
    description :: B.C f Text,
    shortDescription :: B.C f Text,
    label :: B.C f (Maybe Text),
    createdAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MessageTranslationT where
  data PrimaryKey MessageTranslationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . messageId

type MessageTranslation = MessageTranslationT Identity

messageTranslationTMod :: MessageTranslationT (B.FieldModification (B.TableField MessageTranslationT))
messageTranslationTMod =
  B.tableModification
    { messageId = B.fieldNamed "message_id",
      language = B.fieldNamed "language",
      title = B.fieldNamed "title",
      description = B.fieldNamed "description",
      shortDescription = B.fieldNamed "short_description",
      label = B.fieldNamed "label",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''MessageTranslationT ['messageId] [])

$(mkTableInstances ''MessageTranslationT "message_translation" "atlas_driver_offer_bpp")
