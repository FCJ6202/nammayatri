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
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Message.Message where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize hiding (label)
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Message.Message as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.MediaFile (MediaFileTId)
import Storage.Tabular.Merchant (MerchantTId)

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case (readMaybe (unpackChars value')) of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

data MessageT f = MessageT
  { id :: B.C f Text,
    messageType :: B.C f Domain.MessageType,
    title :: B.C f Text,
    description :: B.C f Text,
    shortDescription :: B.C f Text,
    label :: B.C f (Maybe Text),
    likeCount :: B.C f Int,
    mediaFiles :: B.C f [Text],
    merchantId :: B.C f Text,
    createdAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MessageT where
  data PrimaryKey MessageT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta MessageT where
  modelFieldModification = messageTMod
  modelTableName = "message"
  mkExprWithDefault _ = B.insertExpressions []

type Message = MessageT Identity

instance FromJSON Message where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Message where
  toJSON = A.genericToJSON A.defaultOptions

instance HasSqlValueSyntax be String => HasSqlValueSyntax be [Text] where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Text]

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.MessageType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.MessageType

deriving stock instance Show Message

deriving stock instance Ord Domain.MessageType

deriving stock instance Eq Domain.MessageType

messageTMod :: MessageT (B.FieldModification (B.TableField MessageT))
messageTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      messageType = B.fieldNamed "message_type",
      title = B.fieldNamed "title",
      description = B.fieldNamed "description",
      shortDescription = B.fieldNamed "short_description",
      label = B.fieldNamed "label",
      likeCount = B.fieldNamed "like_count",
      mediaFiles = B.fieldNamed "media_files",
      merchantId = B.fieldNamed "merchant_id",
      createdAt = B.fieldNamed "created_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

messageToHSModifiers :: M.Map Text (A.Value -> A.Value)
messageToHSModifiers =
  M.fromList
    []

messageToPSModifiers :: M.Map Text (A.Value -> A.Value)
messageToPSModifiers =
  M.fromList
    []

$(enableKVPG ''MessageT ['id] [])
