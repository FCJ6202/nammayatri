{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Person.PersonFlowStatus where

import Domain.Types.Person
import Domain.Types.Person.PersonFlowStatus
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, MonadFlow)
import qualified Storage.Queries.Person.PersonFlowStatus as Queries

create :: MonadFlow m => PersonFlowStatus -> m ()
create = Queries.create

getStatus :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person -> m (Maybe FlowStatus)
getStatus personId =
  Hedis.safeGet (makeFlowStatusKey personId) >>= \case
    Just a -> return a
    Nothing -> flip whenJust (cachedStatus personId) /=<< Queries.getStatus personId

updateStatus :: MonadFlow m => Id Person -> FlowStatus -> m ()
updateStatus = Queries.updateStatus

deleteByPersonId :: MonadFlow m => Id Person -> m ()
deleteByPersonId = Queries.deleteByPersonId

updateToIdleMultiple :: MonadFlow m => [Id Person] -> UTCTime -> m ()
updateToIdleMultiple = Queries.updateToIdleMultiple

cachedStatus :: CacheFlow m r => Id Person -> FlowStatus -> m ()
cachedStatus personId flowStatus = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let personIdKey = makeFlowStatusKey personId
  Hedis.setExp personIdKey flowStatus expTime

makeFlowStatusKey :: Id Person -> Text
makeFlowStatusKey personId = "CachedQueries:Person:FlowStatus-" <> personId.getId

clearCache :: MonadFlow m => CacheFlow m r => Id Person -> m ()
clearCache personId = do
  Hedis.del (makeFlowStatusKey personId)
