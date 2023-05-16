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

module Storage.Beam.Issue.IssueCategory where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
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
import qualified Domain.Types.Issue.IssueCategory as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.UtilsTH
import Sequelize

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

data IssueCategoryT f = IssueCategoryT
  { id :: B.C f Text,
    category :: B.C f Text,
    logoUrl :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueCategoryT where
  data PrimaryKey IssueCategoryT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta IssueCategoryT where
  modelFieldModification = issueCategoryTMod
  modelTableName = "issue_category"
  mkExprWithDefault _ = B.insertExpressions []

type IssueCategory = IssueCategoryT Identity

instance FromJSON IssueCategory where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON IssueCategory where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show IssueCategory

issueCategoryTMod :: IssueCategoryT (B.FieldModification (B.TableField IssueCategoryT))
issueCategoryTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      category = B.fieldNamed "category",
      logoUrl = B.fieldNamed "logo_url"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

issueCategoryToHSModifiers :: M.Map Text (A.Value -> A.Value)
issueCategoryToHSModifiers =
  M.fromList
    []

issueCategoryToPSModifiers :: M.Map Text (A.Value -> A.Value)
issueCategoryToPSModifiers =
  M.fromList
    []

$(enableKVPG ''IssueCategoryT ['id] [])
