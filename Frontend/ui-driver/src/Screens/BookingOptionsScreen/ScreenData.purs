module Screens.BookingOptionsScreen.ScreenData where

import Screens.Types (BookingOptionsScreenState, RidePreference)
import Services.API as API
import Data.Maybe (Maybe(..))
import ConfigProvider (getAppConfig, appConfig)

initData :: BookingOptionsScreenState
initData =
  { data:
      { vehicleType: ""
      , vehicleNumber: ""
      , vehicleName: ""
      , vehicleCapacity: 0
      , downgradeOptions: []
      , ridePreferences: []
      , defaultRidePreference: defaultRidePreferenceOption
      , canSwitchToRental: false
      , canSwitchToInterCity: false
      , airConditioned: Nothing
      , config: getAppConfig appConfig
      }
  , props:
      { isBtnActive: false
      , downgraded: false
      , canSwitchToRental: false
      , acExplanationPopup: false
      , canSwitchToIntercity : Nothing
      , fromDeepLink : false
      }
  }

defaultRidePreferenceOption :: RidePreference
defaultRidePreferenceOption =
  { airConditioned: Nothing
  , driverRating: Nothing
  , isDefault: false
  , isSelected: false
  , longDescription: Nothing
  , luggageCapacity: Nothing
  , name: ""
  , seatingCapacity: Nothing
  , serviceTierType: API.AUTO_RICKSHAW
  , shortDescription: Nothing
  , vehicleRating: Nothing
  , isUsageRestricted: false
  , priority: 0
  }
