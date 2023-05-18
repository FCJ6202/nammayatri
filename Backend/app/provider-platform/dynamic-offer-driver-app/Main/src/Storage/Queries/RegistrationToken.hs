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
import Domain.Types.RegistrationToken
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
import qualified Storage.Beam.RegistrationToken as BeamRT
import Storage.Tabular.RegistrationToken

create :: RegistrationToken -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id RegistrationToken -> m (Maybe RegistrationToken)
findById = Esq.findById

setVerified :: Id RegistrationToken -> SqlDB ()
setVerified rtId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RegistrationTokenVerified =. val True,
        RegistrationTokenUpdatedAt =. val now
      ]
    where_ $ tbl ^. RegistrationTokenTId ==. val (toKey rtId)

findByToken :: Transactionable m => RegToken -> m (Maybe RegistrationToken)
findByToken token =
  findOne $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenToken ==. val token
    return regToken

updateAttempts :: Int -> Id RegistrationToken -> SqlDB ()
updateAttempts attemps rtId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RegistrationTokenAttempts =. val attemps,
        RegistrationTokenUpdatedAt =. val now
      ]
    where_ $ tbl ^. RegistrationTokenTId ==. val (toKey rtId)

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId personId =
  Esq.delete $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenEntityId ==. val (getId personId)

deleteByPersonIdExceptNew :: Id Person -> Id RegistrationToken -> SqlDB ()
deleteByPersonIdExceptNew personId newRT =
  Esq.delete $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $
      regToken ^. RegistrationTokenEntityId ==. val (getId personId)
        &&. not_ (regToken ^. RegistrationTokenTId ==. val (toKey newRT))

findAllByPersonId :: Transactionable m => Id Person -> m [RegistrationToken]
findAllByPersonId personId =
  findAll $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenEntityId ==. val (getId personId)
    return regToken

getAlternateNumberAttempts :: Transactionable m => Id Person -> m Int
getAlternateNumberAttempts personId =
  fromMaybe 5 . listToMaybe
    <$> Esq.findAll do
      attempts <- from $ table @RegistrationTokenT
      where_ $ attempts ^. RegistrationTokenEntityId ==. val (getId personId)
      return $ attempts ^. RegistrationTokenAlternateNumberAttempts

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
      entityType = entityType,
      createdAt = createdAt,
      updatedAt = updatedAt,
      info = info,
      alternateNumberAttempts = alternateNumberAttempts
    }

transformDomainRegistrationTokenToBeam :: RegistrationToken -> BeamRT.RegistrationToken
transformDomainRegistrationTokenToBeam RegistrationToken {..} =
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
      BeamRT.entityType = entityType,
      BeamRT.createdAt = createdAt,
      BeamRT.updatedAt = updatedAt,
      BeamRT.info = info,
      BeamRT.alternateNumberAttempts = alternateNumberAttempts
    }
