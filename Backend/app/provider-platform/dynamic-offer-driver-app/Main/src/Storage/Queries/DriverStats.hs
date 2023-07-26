{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverStats where

import Domain.Types.DriverStats
import Domain.Types.Person (Driver)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.DriverStats

create :: DriverStats -> SqlDB ()
create = Esq.create

createInitialDriverStats :: Id Driver -> SqlDB ()
createInitialDriverStats driverId = do
  now <- getCurrentTime
  Esq.create $
    DriverStats
      { driverId = driverId,
        idleSince = now,
        totalRides = 0,
        totalEarnings = 0,
        bonusEarned = 0,
        lateNightTrips = 0,
        earningsMissed = 0,
        totalDistance = 0,
        ridesCancelled = Just 0,
        totalRidesAssigned = Just 0
      }

getTopDriversByIdleTime :: Transactionable m => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime count_ ids =
  Esq.findAll $ do
    driverStats <- from $ table @DriverStatsT
    where_ $ driverStats ^. DriverStatsDriverId `in_` valList (toKey . cast <$> ids)
    orderBy [asc $ driverStats ^. DriverStatsIdleSince]
    limit $ fromIntegral count_
    return $ driverStats ^. DriverStatsTId

updateIdleTime :: Id Driver -> SqlDB ()
updateIdleTime driverId = updateIdleTimes [driverId]

updateIdleTimes :: [Id Driver] -> SqlDB ()
updateIdleTimes driverIds = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverStatsIdleSince =. val now
      ]
    where_ $ tbl ^. DriverStatsDriverId `in_` valList (toKey . cast <$> driverIds)

fetchAll :: Transactionable m => m [DriverStats]
fetchAll = Esq.findAll $ from $ table @DriverStatsT

findById :: Transactionable m => Id Driver -> m (Maybe DriverStats)
findById = Esq.findById

deleteById :: Id Driver -> SqlDB ()
deleteById = Esq.deleteByKey @DriverStatsT

incrementTotalRidesAndTotalDist :: Id Driver -> Meters -> SqlDB ()
incrementTotalRidesAndTotalDist driverId rideDist = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverStatsTotalRides =. (tbl ^. DriverStatsTotalRides) +. val 1,
        DriverStatsTotalDistance =. (tbl ^. DriverStatsTotalDistance) +. val rideDist
      ]
    where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)

incrementTotalRidesAssigned :: Id Driver -> Int -> SqlDB ()
incrementTotalRidesAssigned driverId number = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverStatsTotalRidesAssigned =. just (Esq.coalesceDefault [tbl ^. DriverStatsTotalRidesAssigned] (val 0) +. val number)
      ]
    where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)

setDriverStats :: Id Driver -> Int -> Int -> Money -> SqlDB ()
setDriverStats driverId totalRides cancelledCount missedEarning = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverStatsTotalRidesAssigned =. just (Esq.coalesceDefault [tbl ^. DriverStatsTotalRidesAssigned] (val 0) +. val totalRides),
        DriverStatsRidesCancelled =. val (Just cancelledCount),
        DriverStatsEarningsMissed =. (tbl ^. DriverStatsEarningsMissed) +. val missedEarning
      ]
    where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)

setCancelledRidesCountAndIncrementEarningsMissed :: Id Driver -> Int -> Money -> SqlDB () -- check this
setCancelledRidesCountAndIncrementEarningsMissed driverId cancelledCount missedEarning = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverStatsRidesCancelled =. val (Just cancelledCount),
        DriverStatsEarningsMissed =. (tbl ^. DriverStatsEarningsMissed) +. val missedEarning
      ]
    where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)

getDriversSortedOrder :: Transactionable m => Maybe Integer -> m [DriverStats]
getDriversSortedOrder mbLimitVal =
  Esq.findAll $ do
    driverStats <- from $ table @DriverStatsT
    orderBy [desc (driverStats ^. DriverStatsTotalRides), desc (driverStats ^. DriverStatsTotalDistance)]
    limit $ maybe 10 fromIntegral mbLimitVal
    return driverStats

incrementTotalEarningsAndBonusEarned :: Id Driver -> Money -> Money -> SqlDB () -- name them set
incrementTotalEarningsAndBonusEarned driverId increasedEarning increasedBonus = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverStatsTotalEarnings =. (tbl ^. DriverStatsTotalEarnings) +. val increasedEarning,
        DriverStatsBonusEarned =. (tbl ^. DriverStatsBonusEarned) +. val increasedBonus
      ]
    where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)

incrementLateNightTrips :: Id Driver -> Int -> SqlDB ()
incrementLateNightTrips driverId tripCount = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverStatsLateNightTrips =. (tbl ^. DriverStatsLateNightTrips) +. val tripCount
      ]
    where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)
