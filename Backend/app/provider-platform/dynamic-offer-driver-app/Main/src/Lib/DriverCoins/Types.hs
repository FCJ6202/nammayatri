{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Lib.DriverCoins.Types
  ( DriverCoinsEventType (..),
    DriverCoinsFunctionType (..),
    CoinMessage (..),
  )
where

import Domain.Types.Ride
import Kernel.Prelude
import Kernel.Types.Common (Meters)
import qualified Text.Show (show)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data DriverCoinsFunctionType
  = OneOrTwoStarRating
  | RideCompleted
  | FiveStarRating
  | BookingCancellation
  | CustomerReferral
  | DriverReferral
  | TwoRidesCompleted
  | FiveRidesCompleted
  | TenRidesCompleted
  | RidesCompleted Int
  | EightPlusRidesInOneDay
  | PurpleRideCompleted
  | LeaderBoardTopFiveHundred
  | TrainingCompleted
  | BulkUploadFunction
  | BulkUploadFunctionV2 CoinMessage
  | MetroRideCompleted
  deriving (Show, Eq, Read, Generic, FromJSON, ToSchema, ToJSON, Ord, Typeable)

data DriverCoinsEventType
  = Rating {ratingValue :: Int, ride :: Ride}
  | EndRide {isDisabled :: Bool, ride :: Ride, metroRide :: Bool}
  | Cancellation {rideStartTime :: UTCTime, intialDisToPickup :: Maybe Meters, cancellationDisToPickup :: Maybe Meters}
  | DriverToCustomerReferral {ride :: Ride}
  | CustomerToDriverReferral
  | LeaderBoard
  | Training
  | BulkUploadEvent
  deriving (Generic)

driverCoinsEventTypeToString :: DriverCoinsEventType -> String
driverCoinsEventTypeToString = \case
  Rating {} -> "Rating"
  EndRide {} -> "EndRide"
  Cancellation {} -> "Cancellation"
  DriverToCustomerReferral {} -> "DriverToCustomerReferral"
  CustomerToDriverReferral {} -> "CustomerToDriverReferral"
  LeaderBoard -> "LeaderBoard"
  Training -> "Training"
  BulkUploadEvent -> "BulkUploadEvent"

instance Show DriverCoinsEventType where
  show = driverCoinsEventTypeToString

data CoinMessage
  = CoinAdded
  | CoinSubtracted
  | FareRecomputation
  deriving (Show, Eq, Read, Generic, FromJSON, ToSchema, ToJSON, Ord, Typeable)

$(mkBeamInstancesForEnum ''CoinMessage)

$(mkBeamInstancesForEnum ''DriverCoinsFunctionType)
