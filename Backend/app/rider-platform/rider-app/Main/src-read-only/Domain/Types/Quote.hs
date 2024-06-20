{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Quote where

import Data.Aeson
import qualified Domain.Action.UI.DriverOffer
import qualified Domain.Action.UI.SpecialZoneQuote
import qualified Domain.Types.DriverOffer
import qualified Domain.Types.InterCityDetails
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RentalDetails
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.SpecialZoneQuote
import qualified Domain.Types.TripTerms
import qualified Domain.Types.VehicleServiceTier
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Tools.Beam.UtilsTH

data Quote = Quote
  { id :: Kernel.Types.Id.Id Domain.Types.Quote.Quote,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    estimatedFare :: Kernel.Types.Common.Price,
    discount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    estimatedTotalFare :: Kernel.Types.Common.Price,
    estimatedPickupDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    providerId :: Kernel.Prelude.Text,
    providerUrl :: Kernel.Types.Common.BaseUrl,
    itemId :: Kernel.Prelude.Text,
    tripTerms :: Kernel.Prelude.Maybe Domain.Types.TripTerms.TripTerms,
    quoteDetails :: Domain.Types.Quote.QuoteDetails,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    vehicleServiceTierType :: Domain.Types.VehicleServiceTier.VehicleServiceTierType,
    vehicleServiceTierSeatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleServiceTierAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierShortDesc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialLocationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isCustomerPrefferedSearchRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isBlockedRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    tollChargesInfo :: Kernel.Prelude.Maybe Domain.Types.Quote.TollChargesInfo,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data OneWayQuoteAPIDetails = OneWayQuoteAPIDetails
  { distanceToNearestDriver :: Kernel.Types.Common.HighPrecMeters,
    distanceToNearestDriverWithUnit :: Kernel.Types.Common.Distance,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data OneWayQuoteDetails = OneWayQuoteDetails {distanceToNearestDriver :: Kernel.Types.Common.Distance} deriving (Generic, Show)

data OneWaySpecialZoneQuoteAPIDetails = OneWaySpecialZoneQuoteAPIDetails {distanceToNearestDriver :: Kernel.Types.Common.HighPrecMeters, quoteId :: Kernel.Prelude.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuoteAPIDetails
  = OneWayAPIDetails Domain.Types.Quote.OneWayQuoteAPIDetails
  | InterCityAPIDetails Domain.Types.InterCityDetails.InterCityDetailsAPIEntity
  | RentalAPIDetails Domain.Types.RentalDetails.RentalDetailsAPIEntity
  | DriverOfferAPIDetails Domain.Action.UI.DriverOffer.DriverOfferAPIEntity
  | OneWaySpecialZoneAPIDetails Domain.Action.UI.SpecialZoneQuote.SpecialZoneQuoteAPIEntity
  deriving (Generic, Show)

data QuoteDetails
  = OneWayDetails Domain.Types.Quote.OneWayQuoteDetails
  | InterCityDetails Domain.Types.InterCityDetails.InterCityDetails
  | RentalDetails Domain.Types.RentalDetails.RentalDetails
  | DriverOfferDetails Domain.Types.DriverOffer.DriverOffer
  | OneWaySpecialZoneDetails Domain.Types.SpecialZoneQuote.SpecialZoneQuote
  deriving (Generic, Show)

data TollChargesInfo = TollChargesInfo {tollCharges :: Kernel.Types.Common.Price, tollNames :: [Kernel.Prelude.Text]} deriving (Generic, Show)
