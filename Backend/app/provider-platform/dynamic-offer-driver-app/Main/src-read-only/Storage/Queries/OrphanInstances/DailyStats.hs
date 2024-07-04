{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DailyStats where

import qualified Domain.Types.DailyStats
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DailyStats as Beam

instance FromTType' Beam.DailyStats Domain.Types.DailyStats.DailyStats where
  fromTType' (Beam.DailyStatsT {..}) = do
    pure $
      Just
        Domain.Types.DailyStats.DailyStats
          { activatedValidRides = Kernel.Prelude.fromMaybe 0 activatedValidRides,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverId = Kernel.Types.Id.Id driverId,
            id = id,
            merchantLocalDate = merchantLocalDate,
            numRides = numRides,
            payoutOrderId = payoutOrderId,
            payoutOrderStatus = payoutOrderStatus,
            payoutStatus = Kernel.Prelude.fromMaybe Domain.Types.DailyStats.Verifying payoutStatus,
            referralCounts = Kernel.Prelude.fromMaybe 0 referralCounts,
            referralEarnings = Kernel.Types.Common.mkAmountWithDefault referralEarningsAmount referralEarnings,
            totalDistance = totalDistance,
            totalEarnings = Kernel.Types.Common.mkAmountWithDefault totalEarningsAmount totalEarnings,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DailyStats Domain.Types.DailyStats.DailyStats where
  toTType' (Domain.Types.DailyStats.DailyStats {..}) = do
    Beam.DailyStatsT
      { Beam.activatedValidRides = Kernel.Prelude.Just activatedValidRides,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = id,
        Beam.merchantLocalDate = merchantLocalDate,
        Beam.numRides = numRides,
        Beam.payoutOrderId = payoutOrderId,
        Beam.payoutOrderStatus = payoutOrderStatus,
        Beam.payoutStatus = Kernel.Prelude.Just payoutStatus,
        Beam.referralCounts = Kernel.Prelude.Just referralCounts,
        Beam.referralEarnings = Kernel.Prelude.roundToIntegral referralEarnings,
        Beam.referralEarningsAmount = Kernel.Prelude.Just referralEarnings,
        Beam.totalDistance = totalDistance,
        Beam.totalEarnings = Kernel.Prelude.roundToIntegral totalEarnings,
        Beam.totalEarningsAmount = Kernel.Prelude.Just totalEarnings,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
