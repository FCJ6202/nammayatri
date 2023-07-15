{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Estimate where

import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data Estimate = Estimate
  { id :: Id Estimate,
    requestId :: Id DSR.SearchRequest,
    vehicleVariant :: Variant.Variant,
    minFare :: HighPrecMoney,
    maxFare :: HighPrecMoney,
    estimateBreakupList :: [EstimateBreakup],
    nightShiftInfo :: Maybe NightShiftInfo,
    waitingCharges :: WaitingCharges,
    specialLocationTag :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic)

data WaitingCharges = WaitingCharges
  { waitingChargePerMin :: Maybe HighPrecMoney,
    waitingOrPickupCharges :: Maybe HighPrecMoney
  }
  deriving (Generic)

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: HighPrecMoney,
    oldNightShiftCharge :: HighPrecMoney,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic)

data EstimateBreakupD (s :: UsageSafety) = EstimateBreakup
  { title :: Text,
    price :: EstimateBreakupPriceD s
  }
  deriving (Generic)

type EstimateBreakup = EstimateBreakupD 'Safe

deriving instance FromJSON (EstimateBreakupD 'Unsafe)

deriving instance ToJSON (EstimateBreakupD 'Unsafe)

data EstimateBreakupPriceD (s :: UsageSafety) = EstimateBreakupPrice
  { currency :: Text,
    value :: HighPrecMoney
  }
  deriving (Generic)

type EstimateBreakupPrice = EstimateBreakupPriceD 'Safe

deriving instance FromJSON (EstimateBreakupPriceD 'Unsafe)

deriving instance ToJSON (EstimateBreakupPriceD 'Unsafe)
