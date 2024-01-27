{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FarePolicy where

import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Data.Text as T
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FareProduct as FareProductD
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FareProduct as FareProduct
import qualified Storage.CachedQueries.FarePolicy as QFP
import qualified Storage.CachedQueries.FareProduct as QFareProduct
import Tools.Error
import Tools.Maps

data FarePoliciesProduct = FarePoliciesProduct
  { farePolicies :: [FarePolicyD.FullFarePolicy],
    area :: FareProductD.Area,
    specialLocationTag :: Maybe Text
  }

getFarePolicy :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> Variant -> Maybe FareProductD.Area -> m FarePolicyD.FullFarePolicy
getFarePolicy merchantOpCityId tripCategory vehVariant Nothing = do
  fareProduct <- QFareProduct.findByMerchantVariantArea merchantOpCityId tripCategory vehVariant FareProductD.Default >>= fromMaybeM NoFareProduct
  farePolicy <- QFP.findById fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
  return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant fareProduct.tripCategory farePolicy
getFarePolicy merchantOpCityId tripCategory vehVariant (Just area) = do
  mbFareProduct <- QFareProduct.findByMerchantVariantArea merchantOpCityId tripCategory vehVariant area
  case mbFareProduct of
    Just fareProduct -> do
      farePolicy <- QFP.findById fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
      return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant fareProduct.tripCategory farePolicy
    Nothing -> do
      fareProduct <- QFareProduct.findByMerchantVariantArea merchantOpCityId tripCategory vehVariant FareProductD.Default >>= fromMaybeM NoFareProduct
      farePolicy <- QFP.findById fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
      return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant fareProduct.tripCategory farePolicy

getAllFarePoliciesProduct :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> Maybe LatLong -> DTC.TripCategory -> m FarePoliciesProduct
getAllFarePoliciesProduct merchantId merchantOpCityId fromlocaton mbToLocation tripCategory = do
  allFareProducts <- FareProduct.getAllFareProducts merchantId merchantOpCityId fromlocaton mbToLocation tripCategory
  farePolicies <-
    mapM
      ( \fareProduct -> do
          farePolicy <- QFP.findById fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
          return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant fareProduct.tripCategory farePolicy
      )
      allFareProducts.fareProducts
  return $
    FarePoliciesProduct
      { farePolicies,
        area = allFareProducts.area,
        specialLocationTag = allFareProducts.specialLocationTag
      }

mkFarePolicyBreakups :: (Text -> breakupItemValue) -> (Text -> breakupItemValue -> breakupItem) -> Maybe Meters -> FarePolicyD.FarePolicy -> [breakupItem]
mkFarePolicyBreakups mkValue mkBreakupItem mbDistance farePolicy = do
  let distance = fromMaybe 0 mbDistance -- TODO: Fix Later
      driverExtraFeeBounds = FarePolicyD.findDriverExtraFeeBoundsByDistance distance <$> farePolicy.driverExtraFeeBounds
      nightShiftBounds = farePolicy.nightShiftBounds

      driverMinExtraFee = driverExtraFeeBounds <&> (.minFee)
      driverMinExtraFeeCaption = "DRIVER_MIN_EXTRA_FEE"
      driverMinExtraFeeItem = mkBreakupItem driverMinExtraFeeCaption . (mkValue . show . (.getMoney)) <$> driverMinExtraFee

      driverMaxExtraFee = driverExtraFeeBounds <&> (.maxFee)
      driverMaxExtraFeeCaption = "DRIVER_MAX_EXTRA_FEE"
      driverMaxExtraFeeItem = mkBreakupItem driverMaxExtraFeeCaption . (mkValue . show . (.getMoney)) <$> driverMaxExtraFee

      nightShiftStart = nightShiftBounds <&> (.nightShiftStart)
      nightShiftStartCaption = "NIGHT_SHIFT_START"
      nightShiftStartItem = mkBreakupItem nightShiftStartCaption . (mkValue . show) <$> nightShiftStart

      nightShiftEnd = nightShiftBounds <&> (.nightShiftEnd)
      nightShiftEndCaption = "NIGHT_SHIFT_END"
      nightShiftEndItem = mkBreakupItem nightShiftEndCaption . (mkValue . show) <$> nightShiftEnd

      additionalDetailsBreakups = processAdditionalDetails farePolicy.farePolicyDetails
  catMaybes
    [ driverMinExtraFeeItem,
      driverMaxExtraFeeItem,
      nightShiftStartItem,
      nightShiftEndItem
    ]
    <> additionalDetailsBreakups
  where
    processAdditionalDetails = \case
      FarePolicyD.ProgressiveDetails det -> mkAdditionalProgressiveBreakups det
      FarePolicyD.SlabsDetails det -> mkAdditionalSlabBreakups $ FarePolicyD.findFPSlabsDetailsSlabByDistance (fromMaybe 0 mbDistance) det.slabs
      FarePolicyD.RentalDetails _ -> [] -- TODO: Later
    mkAdditionalProgressiveBreakups det = do
      let perExtraKmFareSections = NE.sortBy (comparing (.startDistance)) det.perExtraKmRateSections
          mkPerExtraKmFareItem section = do
            let perExtraKmFareCaption = "EXTRA_PER_KM_FARE_{" `T.append` (T.pack $ show section.startDistance) `T.append` "}"
            mkBreakupItem perExtraKmFareCaption (mkValue $ show section.perExtraKmRate)

          perExtraKmFareItems = mkPerExtraKmFareItem <$> (toList perExtraKmFareSections)

      perExtraKmFareItems
        <> (waitingChargeBreakups det.waitingChargeInfo)
        <> (nightShiftChargeBreakups det.nightShiftCharge)

    mkAdditionalSlabBreakups det = do
      (waitingChargeBreakups det.waitingChargeInfo)
        <> (nightShiftChargeBreakups det.nightShiftCharge)

    waitingChargeBreakups Nothing = []
    waitingChargeBreakups (Just waitingChargeInfo) = do
      let (mbWaitingChargePerMin, mbWaitingOrPickupCharges) =
            waitingChargeInfo.waitingCharge & \case
              FarePolicyD.PerMinuteWaitingCharge hpm -> (Just $ show hpm, Nothing)
              FarePolicyD.ConstantWaitingCharge mo -> (Nothing, Just $ show mo)

          waitingOrPickupChargesCaption = "WAITING_OR_PICKUP_CHARGES"
          mbWaitingOrPickupChargesItem = mkBreakupItem waitingOrPickupChargesCaption . mkValue <$> mbWaitingOrPickupCharges

          mbWaitingChargePerMinCaption = "WAITING_CHARGE_PER_MIN"
          mbWaitingChargePerMinItem = mkBreakupItem mbWaitingChargePerMinCaption . mkValue <$> mbWaitingChargePerMin

      catMaybes [mbWaitingOrPickupChargesItem, mbWaitingChargePerMinItem]

    nightShiftChargeBreakups nightShiftChargeInfo = do
      let getNightShiftChargeValue (FarePolicyD.ProgressiveNightShiftCharge a) = show a
          getNightShiftChargeValue (FarePolicyD.ConstantNightShiftCharge a) = show a

      let oldNightShiftCharge = getNightShiftChargeValue <$> nightShiftChargeInfo
          oldNightShiftChargeCaption = "OLD_NIGHT_SHIFT_CHARGE"
          oldNightShiftChargeItem = mkBreakupItem oldNightShiftChargeCaption . mkValue <$> oldNightShiftCharge

      catMaybes [oldNightShiftChargeItem]
