{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FleetDriverAssociation where

import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data FleetDriverAssociation = FleetDriverAssociation
  { id :: Id FleetDriverAssociation,
    driverId :: Id Person,
    isActive :: Bool,
    fleetOwnerId :: Text,
    associatedOn :: Maybe UTCTime,
    associatedTill :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema, Read, Ord)

makeFleetDriverAssociation :: (MonadFlow m) => Id Person -> Text -> Maybe UTCTime -> m FleetDriverAssociation
makeFleetDriverAssociation driverId fleetOwnerId end = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    FleetDriverAssociation
      { id = id,
        driverId = driverId,
        isActive = True,
        fleetOwnerId = fleetOwnerId,
        associatedOn = Just now,
        associatedTill = end,
        createdAt = now,
        updatedAt = now
      }
