{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSearch where

import Beckn.ACL.Common (getTagV2, validatePrices)
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
-- import Beckn.Types.Core.Taxi.Common.Descriptor as Descriptor -- imported seperately because onsearch has a seperate descriptor
-- import Beckn.Types.Core.Taxi.OnSearch.Item (BreakupItem (..))
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Types.Estimate as DEstimate
import Domain.Types.OnSearchEvent
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (find, id, map, readMaybe, state, unpack)
import GHC.Float (int2Double)
-- import Kernel.External.Maps (LatLong)
import Kernel.Prelude
import Kernel.Product.Validation.Context (validateContext)
-- import Kernel.Storage.Esqueleto (runTransaction)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.DecimalValue as DecimalValue
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.OnSearchEvent as OnSearchEvent
import Tools.Error

buildOnSearchReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    EsqDBFlow m r
  ) =>
  BecknCallbackReq OnSearch.OnSearchMessageV2 ->
  m (Maybe DOnSearch.DOnSearchReq)
buildOnSearchReq req = do
  validateContext Context.ON_SEARCH $ req.context
  logOnSearchEvent req
  case req.contents of
    Right msg -> do
      let catalog = msg.catalog
      Just <$> searchCbService req.context catalog
    Left err -> do
      logTagError "on_search req" $ "on_search error: " <> show err
      pure Nothing

searchCbService :: MonadFlow m => Context.Context -> OnSearch.CatalogV2 -> m DOnSearch.DOnSearchReq
searchCbService context catalog = do
  providerId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
  providerUrl <- context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_uri")
  -- do we need throw an error when we have more than one provider?
  let (provider :| _) = catalog.providers
  let items = provider.items
  (estimatesInfo, quotesInfo) <- partitionEithers <$> traverse (buildEstimateOrQuoteInfo provider) items
  providerName <- fromMaybeM (InvalidRequest "Missing provider info: provider name") $ provider.descriptor.name
  let providerInfo =
        DOnSearch.ProviderInfo
          { providerId = providerId,
            name = providerName,
            url = providerUrl,
            mobileNumber = "", ----------TODO----------Need to remove it or make it maybe in db
            ridesCompleted = 0 ----------TODO----------Need to remove it or make it maybe in db
          }
  let paymentMethodsInfo = [] ----------TODO----------Need to remove it or make it maybe in db
  pure
    DOnSearch.DOnSearchReq
      { requestId = Id context.message_id,
        ..
      }

logOnSearchEvent :: EsqDBFlow m r => OnSearch.OnSearchReqV2 -> m ()
logOnSearchEvent (BecknCallbackReq context (leftToMaybe -> mbErr)) = do
  createdAt <- getCurrentTime
  id <- generateGUID
  bppId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing context.bpp_id")
  let messageId = context.message_id
  let errorType = show.(._type) <$> mbErr
  let errorCode = (.code) <$> mbErr
  let errorMessage = (.message) =<< mbErr
  -- runTransaction $
  void $
    OnSearchEvent.create $
      OnSearchEvent {..}

buildEstimateOrQuoteInfo ::
  (MonadThrow m, Log m) =>
  OnSearch.ProviderV2 ->
  OnSearch.ItemV2 ->
  m (Either DOnSearch.EstimateInfo DOnSearch.QuoteInfo)
buildEstimateOrQuoteInfo provider item = do
  fulfillment <- find (\fulf -> fulf.id == item.fulfillment_id) provider.fulfillments & fromMaybeM (InvalidRequest "Missing fulfillment")
  let itemId = item.id
  let vehicleVariant = castVehicleVariant fulfillment.vehicle.category
      estimatedFare = roundToIntegral item.price.value
      estimatedTotalFare = roundToIntegral item.price.offered_value
      descriptions = []
      nightShiftInfo = buildNightShiftInfo =<< item.tags
      waitingCharges = buildWaitingChargeInfo <$> item.tags
      driversLocation = provider.locations
      specialLocationTag = buildSpecialLocationTag =<< item.tags
  validatePrices estimatedFare estimatedTotalFare
  let totalFareRange =
        DEstimate.FareRange
          { minFare = roundToIntegral item.price.minimum_value,
            maxFare = roundToIntegral item.price.maximum_value
          }
  validateFareRange estimatedTotalFare totalFareRange

  -- if we get here, the discount >= 0, estimatedFare >= estimatedTotalFare
  let discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedFare - estimatedTotalFare
  case fulfillment._type of
    OnSearch.RIDE -> do
      estimateBreakupList <- buildEstimateBreakUpList item
      pure $ Left DOnSearch.EstimateInfo {bppEstimateId = Id fulfillment.id, ..}
    OnSearch.RIDE_OTP -> do
      quoteDetails <- DOnSearch.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneQuoteDetails fulfillment
      pure $ Right DOnSearch.QuoteInfo {..}
  where
    castVehicleVariant = \case
      OnSearch.SEDAN -> VehVar.SEDAN
      OnSearch.SUV -> VehVar.SUV
      OnSearch.HATCHBACK -> VehVar.HATCHBACK
      OnSearch.AUTO_RICKSHAW -> VehVar.AUTO_RICKSHAW
      OnSearch.TAXI -> VehVar.TAXI
      OnSearch.TAXI_PLUS -> VehVar.TAXI_PLUS

buildOneWayQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.ItemV2 ->
  m DOnSearch.OneWayQuoteDetails
buildOneWayQuoteDetails item = do
  distanceToNearestDriver <-
    (item.tags >>= buildDistanceToNearestDriver)
      & fromMaybeM (InvalidRequest "Trip type is ONE_WAY, but distanceToNearestDriver is Nothing")
  pure
    DOnSearch.OneWayQuoteDetails
      { distanceToNearestDriver = realToFrac distanceToNearestDriver
      }

buildOneWaySpecialZoneQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.FulfillmentInfoV2 ->
  m DOnSearch.OneWaySpecialZoneQuoteDetails
buildOneWaySpecialZoneQuoteDetails fulfillment = do
  pure
    DOnSearch.OneWaySpecialZoneQuoteDetails
      { quoteId = fulfillment.id
      }

-- FIXME remove round by using Kilometers and Hours in spec
buildRentalQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DOnSearch.RentalQuoteDetails
buildRentalQuoteDetails _ = do
  baseDistance <- Nothing & fromMaybeM (InvalidRequest "Missing base_distance in rental search item")
  baseDuration <- Nothing & fromMaybeM (InvalidRequest "Missing base_duration in rental search item")
  pure DOnSearch.RentalQuoteDetails {..}

validateFareRange :: (MonadThrow m, Log m) => Money -> DEstimate.FareRange -> m ()
validateFareRange totalFare DEstimate.FareRange {..} = do
  when (minFare < 0) $ throwError $ InvalidRequest "Minimum discounted price is less than zero"
  when (maxFare < 0) $ throwError $ InvalidRequest "Maximum discounted price is less than zero"
  when (maxFare < minFare) $ throwError $ InvalidRequest "Maximum discounted price is less than minimum discounted price"
  when (totalFare > maxFare || totalFare < minFare) $ throwError $ InvalidRequest "Discounted price outside of range"

buildEstimateBreakUpItem ::
  (MonadThrow m, Log m) =>
  Text ->
  OnSearch.TagV2 ->
  m DOnSearch.EstimateBreakupInfo
buildEstimateBreakUpItem currency tag = do
  tagValue <- (readMaybe . T.unpack =<< tag.value) & fromMaybeM (InvalidRequest "Missing fare breakup item : value")
  tagDescriptor <- tag.descriptor & fromMaybeM (InvalidRequest "Missing fare breakup item : descriptor")
  title <- pure tagDescriptor.code -- & fromMaybeM (InvalidRequest "Missing fare breakup item : descriptor code")
  pure
    DOnSearch.EstimateBreakupInfo
      { title = title,
        price =
          DOnSearch.BreakupPriceInfo
            { currency = currency,
              value = Money tagValue
            }
      }

buildEstimateBreakUpList :: (MonadThrow m, Log m) => OnSearch.ItemV2 -> m [DOnSearch.EstimateBreakupInfo]
buildEstimateBreakUpList item = do
  tagGroups <- item.tags & fromMaybeM (InvalidRequest "Missing fare breakup item")

  tagGroup <- find (\tagGroup -> tagGroup.descriptor.code == "fare_breakup") tagGroups & fromMaybeM (InvalidRequest "Missing fare breakup")
  mapM (buildEstimateBreakUpItem item.price.currency) tagGroup.list

buildNightShiftInfo ::
  [OnSearch.TagGroupV2] ->
  Maybe DOnSearch.NightShiftInfo
buildNightShiftInfo itemTags = do
  nightShiftCharge <- getNightShiftCharge itemTags
  oldNightShiftCharge <- getOldNightShiftCharge itemTags
  nightShiftStart <- getNightShiftStart itemTags
  nightShiftEnd <- getNightShiftEnd itemTags
  Just $
    DOnSearch.NightShiftInfo
      { oldNightShiftCharge = realToFrac oldNightShiftCharge,
        ..
      }

buildWaitingChargeInfo' :: [OnSearch.TagGroupV2] -> Maybe Money
buildWaitingChargeInfo' tagGroups = do
  tagValue <- getTagV2 "rate_card" "waiting_charge_per_min" tagGroups
  waitingChargeValue <- readMaybe $ T.unpack tagValue
  Just $ Money waitingChargeValue

buildWaitingChargeInfo :: [OnSearch.TagGroupV2] -> DOnSearch.WaitingChargesInfo
buildWaitingChargeInfo tags = do
  DOnSearch.WaitingChargesInfo
    { waitingChargePerMin = buildWaitingChargeInfo' tags
    }

buildSpecialLocationTag :: [OnSearch.TagGroupV2] -> Maybe Text
buildSpecialLocationTag = getTagV2 "general_info" "special_location_tag"

getNightShiftCharge :: [OnSearch.TagGroupV2] -> Maybe Money
getNightShiftCharge tagGroups = do
  tagValue <- getTagV2 "rate_card" "night_shift_charge" tagGroups
  nightShiftCharge <- readMaybe $ T.unpack tagValue
  Just $ Money nightShiftCharge

getOldNightShiftCharge :: [OnSearch.TagGroupV2] -> Maybe DecimalValue
getOldNightShiftCharge tagGroups = do
  tagValue <- getTagV2 "rate_card" "old_night_shift_charge" tagGroups
  DecimalValue.valueFromString tagValue

buildDistanceToNearestDriver :: [OnSearch.TagGroupV2] -> Maybe DecimalValue
buildDistanceToNearestDriver tagGroups = do
  tagValue <- getTagV2 "general_info" "distance_to_nearest_driver" tagGroups
  distanceToNearestDriver <- readMaybe $ T.unpack tagValue
  Just $ realToFrac $ int2Double distanceToNearestDriver

getNightShiftStart :: [OnSearch.TagGroupV2] -> Maybe TimeOfDay
getNightShiftStart tagGroups = do
  tagValue <- getTagV2 "rate_card" "night_shift_start" tagGroups
  readMaybe $ T.unpack tagValue

getNightShiftEnd :: [OnSearch.TagGroupV2] -> Maybe TimeOfDay
getNightShiftEnd tagGroups = do
  tagValue <- getTagV2 "rate_card" "night_shift_end" tagGroups
  readMaybe $ T.unpack tagValue
