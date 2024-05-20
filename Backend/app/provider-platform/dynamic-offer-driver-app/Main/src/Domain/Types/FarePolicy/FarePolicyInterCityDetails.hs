{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyInterCityDetails where

import Data.Aeson as DA
import Domain.Types.Common
import qualified Domain.Types.FarePolicy.FarePolicyProgressiveDetails as Domain
import Kernel.Prelude
import Kernel.Types.Common

data FPInterCityDetailsD (s :: UsageSafety) = FPInterCityDetails
  { baseFare :: HighPrecMoney,
    perHourCharge :: HighPrecMoney,
    perKmRateOneWay :: HighPrecMoney,
    perKmRateRoundTrip :: HighPrecMoney,
    perExtraKmRate :: HighPrecMoney,
    perExtraMinRate :: HighPrecMoney,
    kmPerPlannedExtraHour :: Kilometers,
    deadKmFare :: HighPrecMoney,
    perDayMaxHourCharge :: HighPrecMoney,
    defaultWaitTimeAtDestination :: Minutes,
    currency :: Currency,
    nightShiftCharge :: Maybe Domain.NightShiftCharge
  }
  deriving (Generic, Show)

type FPInterCityDetails = FPInterCityDetailsD 'Safe

instance FromJSON (FPInterCityDetailsD 'Unsafe)

instance ToJSON (FPInterCityDetailsD 'Unsafe)

-- FIXME remove
instance FromJSON (FPInterCityDetailsD 'Safe)

-- FIXME remove
instance ToJSON (FPInterCityDetailsD 'Safe)
