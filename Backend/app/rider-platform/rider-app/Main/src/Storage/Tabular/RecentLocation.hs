{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RecentLocation where

import Domain.Types.RecentLocation
import qualified Domain.Types.RecentLocation as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

deriving instance Read Domain.RecentLocation

derivePersistField "Domain.RecentLocation"
derivePersistField "Domain.EntityType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RecentLocationT sql=recent_location
      id Text
      riderId Text
      lat Double
      lon Double
      routeCode Text Maybe
      stopCode Text Maybe
      stopLat Double Maybe
      stopLon Double Maybe
      fromStopCode Text Maybe
      fromStopName Text Maybe
      address Text Maybe
      frequency Int
      entityType Domain.EntityType
      routeId Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      merchantOperatingCityId Text
      Primary id
      deriving Generic
    |]

instance TEntityKey RecentLocationT where
  type DomainKey RecentLocationT = Id Domain.RecentLocation
  fromKey (RecentLocationTKey _id) = Id _id
  toKey (Id id) = RecentLocationTKey id

instance FromTType RecentLocationT Domain.RecentLocation where
  fromTType RecentLocationT {..} = do
    return $
      Domain.RecentLocation
        { id = Id id,
          riderId = Id riderId,
          routeCode = routeCode,
          stopCode = stopCode,
          stopLat = stopLat,
          stopLon = stopLon,
          address = address,
          fromStopCode = fromStopCode,
          fromStopName = fromStopName,
          entityType = entityType,
          frequency = frequency,
          routeId = routeId,
          lat = lat,
          lon = lon,
          createdAt = createdAt,
          updatedAt = updatedAt,
          merchantOperatingCityId = Id merchantOperatingCityId
        }
