{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Location where

import Kernel.External.Maps.HasCoordinates
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (PrettyShow)

data Location = Location
  { id :: Id Location,
    lat :: Double,
    lon :: Double,
    address :: LocationAddress,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show, Eq, PrettyShow, HasCoordinates)

data LocationAddress = LocationAddress
  { street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
    fullAddress :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show, Eq, PrettyShow)

buildLocation :: (MonadFlow m) => LatLong -> LocationAddress -> m Location
buildLocation LatLong {..} address = do
  currTime <- getCurrentTime
  locationId <- generateGUID
  return $
    Location
      { id = locationId,
        address =
          LocationAddress
            { street = address.street,
              city = address.city,
              state = address.state,
              country = address.country,
              building = address.building,
              areaCode = address.areaCode,
              area = address.area,
              door = address.door,
              fullAddress = address.fullAddress
            },
        createdAt = currTime,
        updatedAt = currTime,
        ..
      }
