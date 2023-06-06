{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchTry where

import qualified Database.Beam.Query ()
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchTry as Domain
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.SearchTry as BeamST
import Storage.Tabular.SearchTry

-- create :: SearchTry -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => SearchTry -> m (MeshResult ())
create searchTry = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainSearchTryToBeam searchTry)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById :: Transactionable m => Id SearchTry -> m (Maybe SearchTry)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id SearchTry -> m (Maybe SearchTry)
findById (Id searchTry) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamSearchTryToDomain <$>) <$> KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamST.id $ Se.Eq searchTry]
    Nothing -> pure Nothing

-- findLastByRequestId ::
--   (Transactionable m) =>
--   Id SearchRequest ->
--   m (Maybe SearchTry)
-- findLastByRequestId searchReqId = do
--   Esq.findOne $ do
--     searchTryT <- from $ table @SearchTryT
--     where_ $
--       searchTryT ^. SearchTryRequestId ==. val (toKey searchReqId)
--     Esq.orderBy [Esq.desc $ searchTryT ^. SearchTrySearchRepeatCounter]
--     Esq.limit 1
--     return searchTryT

findLastByRequestId ::
  L.MonadFlow m =>
  Id SearchRequest ->
  m (Maybe SearchTry)
findLastByRequestId (Id searchRequest) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      _ <- do
        result <- KV.findAllWithOptionsKVConnector dbConf' Mesh.meshConfig [Se.Is BeamST.id $ Se.Eq searchRequest] (Se.Desc BeamST.searchRepeatCounter) (Just 1) Nothing
        case result of
          Left _ -> pure Nothing
          Right val' ->
            let searchtries = transformBeamSearchTryToDomain <$> val'
             in pure $ headMaybe searchtries
      pure Nothing
    Nothing -> pure Nothing
  where
    headMaybe [] = Nothing
    headMaybe (a : _) = Just a

cancelActiveTriesByRequestId ::
  Id SearchRequest ->
  SqlDB ()
cancelActiveTriesByRequestId searchId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchTryUpdatedAt =. val now,
        SearchTryStatus =. val CANCELLED
      ]
    where_ $
      tbl ^. SearchTryRequestId ==. val (toKey searchId)
        &&. tbl ^. SearchTryStatus ==. val ACTIVE

-- updateStatus ::
--   Id SearchTry ->
--   SearchTryStatus ->
--   SqlDB ()
-- updateStatus searchId status_ = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ SearchTryUpdatedAt =. val now,
--         SearchTryStatus =. val status_
--       ]
--     where_ $ tbl ^. SearchTryTId ==. val (toKey searchId)

updateStatus ::
  (L.MonadFlow m, MonadTime m) =>
  Id SearchTry ->
  SearchTryStatus ->
  m ()
updateStatus (Id searchId) status_ = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          Mesh.meshConfig
          [ Se.Set BeamST.status status_,
            Se.Set BeamST.updatedAt now
          ]
          [Se.Is BeamST.id $ Se.Eq searchId]
    Nothing -> pure ()

getSearchTryStatusAndValidTill ::
  (Transactionable m) =>
  Id SearchTry ->
  m (Maybe (UTCTime, SearchTryStatus))
getSearchTryStatusAndValidTill searchRequestId = do
  findOne $ do
    searchT <- from $ table @SearchTryT
    where_ $
      searchT ^. SearchTryTId ==. val (toKey searchRequestId)
    return (searchT ^. SearchTryValidTill, searchT ^. SearchTryStatus)

transformBeamSearchTryToDomain :: BeamST.SearchTry -> SearchTry
transformBeamSearchTryToDomain BeamST.SearchTryT {..} = do
  SearchTry
    { id = Id id,
      requestId = Id requestId,
      estimateId = Id estimateId,
      messageId = messageId,
      startTime = startTime,
      validTill = validTill,
      vehicleVariant = vehicleVariant,
      baseFare = baseFare,
      customerExtraFee = customerExtraFee,
      status = status,
      searchRepeatCounter = searchRepeatCounter,
      searchRepeatType = searchRepeatType,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainSearchTryToBeam :: SearchTry -> BeamST.SearchTry
transformDomainSearchTryToBeam SearchTry {..} =
  BeamST.SearchTryT
    { id = getId id,
      requestId = getId requestId,
      estimateId = getId estimateId,
      messageId = messageId,
      startTime = startTime,
      validTill = validTill,
      vehicleVariant = vehicleVariant,
      baseFare = baseFare,
      customerExtraFee = customerExtraFee,
      status = status,
      searchRepeatCounter = searchRepeatCounter,
      searchRepeatType = searchRepeatType,
      createdAt = createdAt,
      updatedAt = updatedAt
    }
