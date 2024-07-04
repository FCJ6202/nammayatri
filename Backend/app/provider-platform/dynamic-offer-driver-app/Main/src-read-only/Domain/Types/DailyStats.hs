{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DailyStats where

import Data.Aeson
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Types.Person
import qualified Kernel.External.Payout.Juspay.Types.Payout
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DailyStats = DailyStats
  { activatedValidRides :: Kernel.Prelude.Int,
    currency :: Kernel.Types.Common.Currency,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Data.Text.Text,
    merchantLocalDate :: Data.Time.Calendar.Day,
    numRides :: Kernel.Prelude.Int,
    payoutOrderId :: Kernel.Prelude.Maybe Data.Text.Text,
    payoutOrderStatus :: Kernel.Prelude.Maybe Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus,
    payoutStatus :: Domain.Types.DailyStats.PayoutStatus,
    referralCounts :: Kernel.Prelude.Int,
    referralEarnings :: Kernel.Types.Common.HighPrecMoney,
    totalDistance :: Kernel.Types.Common.Meters,
    totalEarnings :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data PayoutStatus = Verifying | Processing | Success | Failed | ManualReview deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutStatus)
