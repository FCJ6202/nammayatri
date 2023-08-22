{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Maps.DirectionsCache
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Maps.DirectionsCache
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.Maps.DirectionsCache as BeamDC

create :: MonadFlow m => DirectionsCache -> m ()
create = createWithKV

findRoute :: MonadFlow m => Text -> Text -> Int -> m (Maybe DirectionsCache)
findRoute originHash destHash slot = findOneWithKV [Se.And [Se.Is BeamDC.originHash $ Se.Eq originHash, Se.Is BeamDC.destHash $ Se.Eq destHash, Se.Is BeamDC.slot $ Se.Eq slot]]

instance FromTType' BeamDC.DirectionsCache DirectionsCache where
  fromTType' BeamDC.DirectionsCacheT {..} = do
    pure $
      Just
        DirectionsCache
          { id = Id id,
            originHash = originHash,
            destHash = destHash,
            slot = slot,
            response = response,
            createdAt = createdAt
          }

instance ToTType' BeamDC.DirectionsCache DirectionsCache where
  toTType' DirectionsCache {..} =
    BeamDC.DirectionsCacheT
      { BeamDC.id = getId id,
        BeamDC.originHash = originHash,
        BeamDC.destHash = destHash,
        BeamDC.slot = slot,
        BeamDC.response = response,
        BeamDC.createdAt = createdAt
      }
