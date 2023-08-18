{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnTrack
  ( onTrack,
    OnTrackReq (..),
    validateRequest,
  )
where

import Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data OnTrackReq = OnTrackReq
  { bppRideId :: Id BPPRide,
    trackUrl :: BaseUrl
  }

data ValidatedOnTrackReq = ValidatedOnTrackReq
  { bppRideId :: Id BPPRide,
    trackUrl :: BaseUrl,
    ride :: Ride
  }

onTrack :: (CacheFlow m r, EsqDBFlow m r) => ValidatedOnTrackReq -> m ()
onTrack ValidatedOnTrackReq {..} = void $ QRide.updateTrackingUrl ride.id trackUrl

validateRequest :: (CacheFlow m r, EsqDBFlow m r) => OnTrackReq -> m ValidatedOnTrackReq
validateRequest OnTrackReq {..} = do
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId:" <> bppRideId.getId)
  return $
    ValidatedOnTrackReq
      { ..
      }
