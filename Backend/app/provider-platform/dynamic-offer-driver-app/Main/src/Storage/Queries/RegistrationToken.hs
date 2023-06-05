{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.RegistrationToken where

import Domain.Types.Person
import Domain.Types.RegistrationToken as DRT
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.RegistrationToken as BeamRT

-- create :: RegistrationToken -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => DRT.RegistrationToken -> m (MeshResult ())
create registrationToken = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainRegistrationTokenToBeam registrationToken)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById :: Transactionable m => Id RegistrationToken -> m (Maybe RegistrationToken)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id RegistrationToken -> m (Maybe RegistrationToken)
findById (Id registrationTokenId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRegistrationTokenToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamRT.id $ Se.Eq registrationTokenId]
    Nothing -> pure Nothing

-- setVerified :: Id RegistrationToken -> SqlDB ()
-- setVerified rtId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RegistrationTokenVerified =. val True,
--         RegistrationTokenUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. RegistrationTokenTId ==. val (toKey rtId)

setVerified :: (L.MonadFlow m, MonadTime m) => Id RegistrationToken -> m (MeshResult ())
setVerified (Id rtId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamRT.verified True,
          Se.Set BeamRT.updatedAt now
        ]
        [Se.Is BeamRT.id (Se.Eq rtId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- findByToken :: Transactionable m => RegToken -> m (Maybe RegistrationToken)
-- findByToken token =
--   findOne $ do
--     regToken <- from $ table @RegistrationTokenT
--     where_ $ regToken ^. RegistrationTokenToken ==. val token
--     return regToken

findByToken :: L.MonadFlow m => RegToken -> m (Maybe RegistrationToken)
findByToken token = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRegistrationTokenToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamRT.token $ Se.Eq token]
    Nothing -> pure Nothing

-- updateAttempts :: Int -> Id RegistrationToken -> SqlDB ()
-- updateAttempts attemps rtId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RegistrationTokenAttempts =. val attemps,
--         RegistrationTokenUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. RegistrationTokenTId ==. val (toKey rtId)

updateAttempts :: (L.MonadFlow m, MonadTime m) => Int -> Id RegistrationToken -> m (MeshResult ())
updateAttempts attempts (Id rtId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamRT.attempts attempts,
          Se.Set BeamRT.updatedAt now
        ]
        [Se.Is BeamRT.id (Se.Eq rtId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- deleteByPersonId :: Id Person -> SqlDB ()
-- deleteByPersonId personId =
--   Esq.delete $ do
--     regToken <- from $ table @RegistrationTokenT
--     where_ $ regToken ^. RegistrationTokenEntityId ==. val (getId personId)

deleteByPersonId :: L.MonadFlow m => Id Person -> m ()
deleteByPersonId (Id personId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamRT.entityId (Se.Eq personId)]
    Nothing -> pure ()

-- deleteByPersonIdExceptNew :: Id Person -> Id RegistrationToken -> SqlDB ()
-- deleteByPersonIdExceptNew personId newRT =
--   Esq.delete $ do
--     regToken <- from $ table @RegistrationTokenT
--     where_ $
--       regToken ^. RegistrationTokenEntityId ==. val (getId personId)
--         &&. not_ (regToken ^. RegistrationTokenTId ==. val (toKey newRT))

deleteByPersonIdExceptNew :: L.MonadFlow m => Id Person -> Id RegistrationToken -> m ()
deleteByPersonIdExceptNew (Id personId) (Id newRT) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.And [Se.Is BeamRT.entityId (Se.Eq personId), Se.Is BeamRT.id (Se.Eq newRT)]]
    Nothing -> pure ()

-- findAllByPersonId :: Transactionable m => Id Person -> m [RegistrationToken]
-- findAllByPersonId personId =
--   findAll $ do
--     regToken <- from $ table @RegistrationTokenT
--     where_ $ regToken ^. RegistrationTokenEntityId ==. val (getId personId)
--     return regToken

findAllByPersonId :: L.MonadFlow m => Id Person -> m [RegistrationToken]
findAllByPersonId personId = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamRegistrationTokenToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamRT.entityId $ Se.Eq $ getId personId]
    Nothing -> pure []

-- getAlternateNumberAttempts :: Transactionable m => Id Person -> m Int
-- getAlternateNumberAttempts personId =
--   fromMaybe 5 . listToMaybe
--     <$> Esq.findAll do
--       attempts <- from $ table @RegistrationTokenT
--       where_ $ attempts ^. RegistrationTokenEntityId ==. val (getId personId)
--       return $ attempts ^. RegistrationTokenAlternateNumberAttempts

getAlternateNumberAttempts :: L.MonadFlow m => Id Person -> m Int
getAlternateNumberAttempts (Id personId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      rt <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamRT.entityId $ Se.Eq personId]
      case rt of
        Left _ -> pure 0
        Right Nothing -> pure 0
        Right (Just x) -> do
          let rt' = transformBeamRegistrationTokenToDomain x
          let attempts = DRT.attempts rt'
          pure attempts
    Nothing -> pure 0

transformBeamRegistrationTokenToDomain :: BeamRT.RegistrationToken -> RegistrationToken
transformBeamRegistrationTokenToDomain BeamRT.RegistrationTokenT {..} = do
  RegistrationToken
    { id = Id id,
      token = token,
      attempts = attempts,
      authMedium = authMedium,
      authType = authType,
      authValueHash = authValueHash,
      verified = verified,
      authExpiry = authExpiry,
      tokenExpiry = tokenExpiry,
      entityId = entityId,
      merchantId = merchantId,
      entityType = entityType,
      createdAt = createdAt,
      updatedAt = updatedAt,
      info = info,
      alternateNumberAttempts = alternateNumberAttempts
    }

transformDomainRegistrationTokenToBeam :: RegistrationToken -> BeamRT.RegistrationToken
transformDomainRegistrationTokenToBeam RegistrationToken {..} =
  BeamRT.defaultRegistrationToken
    { BeamRT.id = getId id,
      BeamRT.token = token,
      BeamRT.attempts = attempts,
      BeamRT.authMedium = authMedium,
      BeamRT.authType = authType,
      BeamRT.authValueHash = authValueHash,
      BeamRT.verified = verified,
      BeamRT.authExpiry = authExpiry,
      BeamRT.tokenExpiry = tokenExpiry,
      BeamRT.entityId = entityId,
      BeamRT.merchantId = merchantId,
      BeamRT.entityType = entityType,
      BeamRT.createdAt = createdAt,
      BeamRT.updatedAt = updatedAt,
      BeamRT.info = info,
      BeamRT.alternateNumberAttempts = alternateNumberAttempts
    }
