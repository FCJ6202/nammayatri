{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RegistrationToken where

import Domain.Types.Person
import Domain.Types.RegistrationToken
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.RegistrationToken as BeamRT

create :: (L.MonadFlow m, Log m) => RegistrationToken -> m ()
create = createWithKV

findById :: (L.MonadFlow m, Log m) => Id RegistrationToken -> m (Maybe RegistrationToken)
findById (Id registrationTokenId) = findOneWithKV [Se.Is BeamRT.id $ Se.Eq registrationTokenId]

findByToken :: (L.MonadFlow m, Log m) => RegToken -> m (Maybe RegistrationToken)
findByToken token = findOneWithKV [Se.Is BeamRT.token $ Se.Eq token]

setVerified :: (L.MonadFlow m, MonadTime m, Log m) => Id RegistrationToken -> m ()
setVerified (Id rtId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamRT.verified True,
      Se.Set BeamRT.updatedAt now
    ]
    [Se.Is BeamRT.id (Se.Eq rtId)]

setDirectAuth :: (L.MonadFlow m, MonadTime m, Log m) => Id RegistrationToken -> m ()
setDirectAuth (Id rtId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamRT.verified True,
      Se.Set BeamRT.authMedium SIGNATURE,
      Se.Set BeamRT.authType DIRECT,
      Se.Set BeamRT.updatedAt now
    ]
    [Se.Is BeamRT.id (Se.Eq rtId)]

updateAttempts :: (L.MonadFlow m, MonadTime m, Log m) => Int -> Id RegistrationToken -> m ()
updateAttempts attempts (Id rtId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamRT.attempts attempts,
      Se.Set BeamRT.updatedAt now
    ]
    [Se.Is BeamRT.id (Se.Eq rtId)]

deleteByPersonId :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.And [Se.Is BeamRT.entityId (Se.Eq personId)]]

deleteByPersonIdExceptNew :: (L.MonadFlow m, Log m) => Id Person -> Id RegistrationToken -> m ()
deleteByPersonIdExceptNew (Id personId) (Id newRT) = deleteWithKV [Se.And [Se.Is BeamRT.entityId (Se.Eq personId), Se.Is BeamRT.id (Se.Not $ Se.Eq newRT)]]

findAllByPersonId :: (L.MonadFlow m, Log m) => Id Person -> m [RegistrationToken]
findAllByPersonId personId = findAllWithKV [Se.Is BeamRT.entityId $ Se.Eq $ getId personId]

instance FromTType' BeamRT.RegistrationToken RegistrationToken where
  fromTType' BeamRT.RegistrationTokenT {..} = do
    pure $
      Just
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
            info = info
          }

instance ToTType' BeamRT.RegistrationToken RegistrationToken where
  toTType' RegistrationToken {..} = do
    BeamRT.RegistrationTokenT
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
        BeamRT.info = info
      }
