{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.Types.Booking.Type
  ( module Domain.Types.Booking.Type,
    PaymentStatus (..),
  )
where

import BecknV2.OnDemand.Enums (PaymentStatus (..))
import Data.Aeson
import qualified Domain.Types.Client as DC
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH

activeBookingStatus :: [BookingStatus]
activeBookingStatus = [NEW, CONFIRMED, AWAITING_REASSIGNMENT, TRIP_ASSIGNED]

activeScheduledBookingStatus :: [BookingStatus]
activeScheduledBookingStatus = [AWAITING_REASSIGNMENT, TRIP_ASSIGNED]

data BookingStatus
  = NEW
  | CONFIRMED
  | AWAITING_REASSIGNMENT
  | REALLOCATED
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance CH.ClickhouseValue BookingStatus

$(mkBeamInstancesForEnum ''BookingStatus)

$(mkHttpInstancesForEnum ''BookingStatus)

deriving instance Ord PaymentStatus

$(mkBeamInstancesForEnum ''PaymentStatus)

$(mkHttpInstancesForEnum ''PaymentStatus)

data BPPBooking

data Booking = Booking
  { id :: Id Booking,
    transactionId :: Text,
    fulfillmentId :: Maybe Text,
    clientId :: Maybe (Id DC.Client),
    bppBookingId :: Maybe (Id BPPBooking),
    quoteId :: Maybe (Id DQuote.Quote),
    paymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod),
    paymentUrl :: Maybe Text,
    status :: BookingStatus,
    providerId :: Text,
    providerUrl :: BaseUrl,
    itemId :: Text,
    primaryExophone :: Text,
    startTime :: UTCTime,
    riderId :: Id DPerson.Person,
    fromLocation :: DLoc.Location,
    initialPickupLocation :: DLoc.Location,
    estimatedFare :: Price,
    estimatedDistance :: Maybe HighPrecMeters,
    estimatedDuration :: Maybe Seconds,
    discount :: Maybe Price,
    estimatedTotalFare :: Price,
    isScheduled :: Bool,
    vehicleServiceTierType :: DVST.VehicleServiceTierType,
    bookingDetails :: BookingDetails,
    tripTerms :: Maybe DTripTerms.TripTerms,
    merchantId :: Id DMerchant.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    specialLocationTag :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    serviceTierName :: Maybe Text,
    serviceTierShortDesc :: Maybe Text,
    paymentStatus :: Maybe PaymentStatus,
    clientBundleVersion :: Maybe Version,
    clientSdkVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text
  }
  deriving (Generic, Show)

data BookingDetails
  = OneWayDetails OneWayBookingDetails
  | RentalDetails RentalBookingDetails
  | DriverOfferDetails OneWayBookingDetails
  | OneWaySpecialZoneDetails OneWaySpecialZoneBookingDetails
  | InterCityDetails InterCityBookingDetails
  deriving (Show)

newtype RentalBookingDetails = RentalBookingDetails
  { stopLocation :: Maybe DLoc.Location
  }
  deriving (Show)

data OneWayBookingDetails = OneWayBookingDetails
  { toLocation :: DLoc.Location,
    distance :: HighPrecMeters
  }
  deriving (Show)

data OneWaySpecialZoneBookingDetails = OneWaySpecialZoneBookingDetails
  { toLocation :: DLoc.Location,
    distance :: HighPrecMeters,
    otpCode :: Maybe Text
  }
  deriving (Show)

data InterCityBookingDetails = InterCityBookingDetails
  { toLocation :: DLoc.Location,
    distance :: HighPrecMeters
  }
  deriving (Show)
