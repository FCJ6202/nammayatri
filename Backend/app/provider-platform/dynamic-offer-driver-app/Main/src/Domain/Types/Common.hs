{-# LANGUAGE DeriveAnyClass #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types.Common where

import Control.Lens.Operators hiding ((.=))
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import Data.OpenApi hiding (name)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Domain.Types.ServiceTierType
import EulerHS.Prelude hiding (length)
import Kernel.Prelude
import Kernel.Utils.GenericPretty
import Servant
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data UsageSafety = Safe | Unsafe

data TripCategory = OneWay OneWayMode | Rental RentalMode | RideShare RideShareMode | InterCity OneWayMode (Maybe Text) | CrossCity OneWayMode (Maybe Text)
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToSchema)

-- This is done to handle backward compatibility, as UI is expected "contents" to be a string but due to multiple in InterCity and CrossCity, it got changed into an array
instance ToJSON TripCategory where
  toJSON (OneWay mode) =
    object
      [ "tag" .= ("OneWay" :: Text),
        "contents" .= mode
      ]
  toJSON (Rental mode) =
    object
      [ "tag" .= ("Rental" :: Text),
        "contents" .= mode
      ]
  toJSON (RideShare mode) =
    object
      [ "tag" .= ("RideShare" :: Text),
        "contents" .= mode
      ]
  toJSON (InterCity mode text) =
    object
      [ "tag" .= ("InterCity" :: Text),
        "contents" .= mode,
        "city" .= text
      ]
  toJSON (CrossCity mode text) =
    object
      [ "tag" .= ("CrossCity" :: Text),
        "contents" .= mode,
        "city" .= text
      ]

instance FromJSON TripCategory where
  parseJSON = withObject "TripCategory" $ \v -> do
    tag <- v .: "tag"
    case tag of
      "OneWay" -> OneWay <$> v .: "contents"
      "Rental" -> Rental <$> v .: "contents"
      "RideShare" -> RideShare <$> v .: "contents"
      "InterCity" -> InterCity <$> v .: "contents" <*> v .:? "city"
      "CrossCity" -> CrossCity <$> v .: "contents" <*> v .:? "city"
      _ -> fail $ "Unknown tag: " ++ tag

data TripOption = TripOption
  { schedule :: UTCTime,
    isScheduled :: Bool,
    tripCategories :: [TripCategory]
  }

data OneWayMode = OneWayRideOtp | OneWayOnDemandStaticOffer | OneWayOnDemandDynamicOffer
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable OneWayMode

type RentalMode = TripMode

type RideShareMode = TripMode

data TripMode = RideOtp | OnDemandStaticOffer
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable TripMode

instance FromHttpApiData TripMode where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData TripMode where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance FromHttpApiData OneWayMode where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData OneWayMode where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

$(mkBeamInstancesForEnum ''TripCategory)

instance Show TripCategory where
  show (OneWay s) = "OneWay_" <> show s
  show (Rental s) = "Rental_" <> show s
  show (RideShare s) = "RideShare_" <> show s
  show (InterCity s Nothing) = "InterCity_" <> show s
  show (InterCity s (Just city)) = "InterCity_" <> show s <> "_" <> T.unpack city
  show (CrossCity s Nothing) = "CrossCity_" <> show s
  show (CrossCity s (Just city)) = "CrossCity_" <> show s <> "_" <> T.unpack city

instance ToParamSchema TripCategory where
  toParamSchema _ =
    mempty
      & title ?~ "TripCategory"
      & type_ ?~ OpenApiString
      & enum_
        ?~ [ "OneWay_RideOtp",
             "OneWay_OnDemandStaticOffer",
             "OneWay_OnDemandDynamicOffer",
             "RoundTrip_RideOtp",
             "RoundTrip_OnDemandStaticOffer",
             "Rental_RideOtp",
             "Rental_OnDemandStaticOffer",
             "RideShare_RideOtp",
             "RideShare_OnDemandStaticOffer",
             "InterCity_RideOtp",
             "InterCity_OnDemandStaticOffer",
             "InterCity_OnDemandDynamicOffer",
             "CrossCity_RideOtp",
             "CrossCity_OnDemandStaticOffer",
             "CrossCity_OnDemandDynamicOffer"
           ]

instance Read TripCategory where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (OneWay v1, r2)
            | r1 <- stripPrefix "OneWay_" r,
              (v1, r2) <- readsPrec (app_prec + 1) r1
          ]
            ++ [ (Rental v1, r2)
                 | r1 <- stripPrefix "Rental_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RideShare v1, r2)
                 | r1 <- stripPrefix "RideShare_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (InterCity v1 Nothing, r3)
                 | r1 <- stripPrefix "InterCity_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1,
                   r3 <- [r2]
               ]
            ++ [ (InterCity OneWayRideOtp (Just v1), [])
                 | r1 <- stripPrefix "InterCity_OneWayRideOtp_" r,
                   let v1 = T.pack r1
               ]
            ++ [ (InterCity OneWayOnDemandStaticOffer (Just v1), [])
                 | r1 <- stripPrefix "InterCity_OneWayOnDemandStaticOffer_" r,
                   let v1 = T.pack r1
               ]
            ++ [ (InterCity OneWayOnDemandDynamicOffer (Just v1), [])
                 | r1 <- stripPrefix "InterCity_OneWayOnDemandDynamicOffer_" r,
                   let v1 = T.pack r1
               ]
            ++ [ (CrossCity v1 Nothing, r3)
                 | r1 <- stripPrefix "CrossCity_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1,
                   r3 <- [r2]
               ]
            ++ [ (CrossCity OneWayRideOtp (Just v1), [])
                 | r1 <- stripPrefix "CrossCity_OneWayRideOtp_" r,
                   let v1 = T.pack r1
               ]
            ++ [ (CrossCity OneWayOnDemandStaticOffer (Just v1), [])
                 | r1 <- stripPrefix "CrossCity_OneWayOnDemandStaticOffer_" r,
                   let v1 = T.pack r1
               ]
            ++ [ (CrossCity OneWayOnDemandDynamicOffer (Just v1), [])
                 | r1 <- stripPrefix "CrossCity_OneWayOnDemandDynamicOffer_" r,
                   let v1 = T.pack r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

instance FromHttpApiData TripCategory where
  parseQueryParam = readEither

instance ToHttpApiData TripCategory where
  toUrlPiece = show

isRideOtpBooking :: TripCategory -> Bool
isRideOtpBooking (OneWay OneWayRideOtp) = True
isRideOtpBooking (Rental RideOtp) = True
isRideOtpBooking (RideShare RideOtp) = True
isRideOtpBooking _ = False

-- Move it to configs later if required
isEndOtpRequired :: TripCategory -> Bool
isEndOtpRequired (Rental _) = True
isEndOtpRequired (InterCity _ _) = True
isEndOtpRequired _ = False

-- Move it to configs later if required
isOdometerReadingsRequired :: TripCategory -> Bool
isOdometerReadingsRequired (Rental _) = False
isOdometerReadingsRequired _ = False

-- Move it to configs later if required
isGoHomeAvailable :: TripCategory -> Bool
isGoHomeAvailable (OneWay _) = True
isGoHomeAvailable _ = False

shouldRectifyDistantPointsSnapToRoadFailure :: TripCategory -> Bool
shouldRectifyDistantPointsSnapToRoadFailure tripCategory = case tripCategory of
  Rental _ -> True
  _ -> False

isRentalTrip :: TripCategory -> Bool
isRentalTrip tripCategory = case tripCategory of
  Rental _ -> True
  _ -> False

isFixedNightCharge :: TripCategory -> Bool
isFixedNightCharge tripCategory = isRentalTrip tripCategory || isInterCityTrip tripCategory

isInterCityTrip :: TripCategory -> Bool
isInterCityTrip tripCategory = case tripCategory of
  InterCity _ _ -> True
  _ -> False

isDynamicOfferTrip :: TripCategory -> Bool
isDynamicOfferTrip (OneWay OneWayOnDemandDynamicOffer) = True
isDynamicOfferTrip (CrossCity OneWayOnDemandDynamicOffer _) = True
isDynamicOfferTrip (InterCity OneWayOnDemandDynamicOffer _) = True
isDynamicOfferTrip _ = False

isTollApplicableForTrip :: ServiceTierType -> TripCategory -> Bool
isTollApplicableForTrip AUTO_RICKSHAW _ = False
isTollApplicableForTrip _ (Rental _) = False
isTollApplicableForTrip _ (InterCity _ _) = False
isTollApplicableForTrip _ _ = True
