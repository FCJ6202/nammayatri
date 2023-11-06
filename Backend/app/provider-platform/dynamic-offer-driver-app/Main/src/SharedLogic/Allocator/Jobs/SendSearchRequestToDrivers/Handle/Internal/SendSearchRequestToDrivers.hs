{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers
  ( sendSearchRequestToDrivers,
  )
where

import Control.Monad.Extra (anyM)
import qualified Data.Map as M
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Location as DLoc
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry as DST
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool (getPoolBatchNum)
import SharedLogic.DriverPool
import SharedLogic.GoogleTranslate
import qualified Storage.CachedQueries.BapMetadata as CQSM
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Maps as Maps
import qualified Tools.Notifications as Notify

type LanguageDictionary = M.Map Maps.Language DSR.SearchRequest

sendSearchRequestToDrivers ::
  ( Log m,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    TranslateFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  DSR.SearchRequest ->
  SearchDetails ->
  Maybe DFP.DriverExtraFeeBounds ->
  DriverPoolConfig ->
  [DriverPoolWithActualDistResult] ->
  [Id Driver] ->
  GoHomeConfig ->
  m ()
sendSearchRequestToDrivers searchReq searchDetails driverExtraFeeBounds driverPoolConfig driverPool prevBatchDrivers goHomeConfig = do
  logInfo $ "Send search requests to driver pool batch-" <> show driverPool
  bapMetadata <- CQSM.findById (Id searchReq.bapId)
  now <- getCurrentTime
  let (searchTry', vehVariant, searchReqTag, mbBookingId, startTime, singleBatchProcessTime) = case searchDetails of
        OnDemandDetails OnDemandSearchDetails {searchTry} -> do
          let singleBatchProcessTime' = driverPoolConfig.singleBatchProcessTime
          (searchTry, searchTry.vehicleVariant, DSR.ON_DEMAND, Nothing, searchTry.startTime, singleBatchProcessTime')
        RentalDetails RentalSearchDetails {booking, searchTry} -> do
          let singleBatchProcessTime' = driverPoolConfig.singleBatchProcessTimeRental
          (searchTry, booking.vehicleVariant, DSR.RENTAL, Just booking.id, booking.startTime, singleBatchProcessTime')
  let searchTryId = searchTry'.id
  let validTill = fromIntegral singleBatchProcessTime `addUTCTime` now
  batchNumber <- getPoolBatchNum searchTryId
  languageDictionary <- foldM (addLanguageToDictionary searchReq) M.empty driverPool
  DS.driverScoreEventHandler
    searchReq.merchantOperatingCityId
    DST.OnNewSearchRequestForDrivers
      { driverPool = driverPool,
        merchantId = searchReq.providerId,
        searchReq = searchReq,
        searchTry = searchTry',
        validTill = validTill,
        batchProcessTime = fromIntegral singleBatchProcessTime
      }
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver batchNumber validTill searchReqTag searchTryId mbBookingId startTime) driverPool
  let driverPoolZipSearchRequests = zip driverPool searchRequestsForDrivers
  whenM (anyM (\driverId -> CQDGR.getDriverGoHomeRequestInfo driverId searchReq.merchantOperatingCityId (Just goHomeConfig) <&> isNothing . (.status)) prevBatchDrivers) $
    QSRD.setInactiveBySTId searchTryId -- inactive previous request by drivers so that they can make new offers.
  _ <- QSRD.createMany searchRequestsForDrivers

  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) -> do
    let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
    let translatedSearchReq = fromMaybe searchReq $ M.lookup language languageDictionary
    let entityData = makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq searchDetails bapMetadata dPoolRes.intelligentScores.rideRequestPopupDelayDuration dPoolRes.keepHiddenForSeconds vehVariant

    Notify.notifyOnNewSearchRequestAvailable searchReq.merchantOperatingCityId sReqFD.driverId dPoolRes.driverPoolResult.driverDeviceToken entityData
  where
    buildSearchRequestForDriver ::
      ( MonadFlow m,
        Redis.HedisFlow m r
      ) =>
      Int ->
      UTCTime ->
      DSR.SearchRequestTag ->
      Id DST.SearchTry ->
      Maybe (Id DB.Booking) ->
      UTCTime ->
      DriverPoolWithActualDistResult ->
      m SearchRequestForDriver
    buildSearchRequestForDriver batchNumber validTill searchRequestTag searchTryId bookingId startTime dpwRes = do
      guid <- generateGUID
      now <- getCurrentTime
      let dpRes = dpwRes.driverPoolResult
      parallelSearchRequestCount <- Just <$> getValidSearchRequestCount searchReq.providerId dpRes.driverId now
      let searchRequestForDriver =
            SearchRequestForDriver
              { id = guid,
                requestId = searchReq.id,
                searchRequestTag,
                searchTryId,
                bookingId,
                startTime,
                merchantId = Just searchReq.providerId,
                merchantOperatingCityId = searchReq.merchantOperatingCityId,
                searchRequestValidTill = validTill,
                driverId = cast dpRes.driverId,
                vehicleVariant = dpRes.variant,
                actualDistanceToPickup = dpwRes.actualDistanceToPickup,
                straightLineDistanceToPickup = dpRes.distanceToPickup,
                durationToPickup = dpwRes.actualDurationToPickup,
                status = Active,
                lat = Just dpRes.lat,
                lon = Just dpRes.lon,
                createdAt = now,
                response = Nothing,
                driverMinExtraFee = driverExtraFeeBounds <&> (.minFee),
                driverMaxExtraFee = driverExtraFeeBounds <&> (.maxFee),
                rideRequestPopupDelayDuration = dpwRes.intelligentScores.rideRequestPopupDelayDuration,
                isPartOfIntelligentPool = dpwRes.isPartOfIntelligentPool,
                acceptanceRatio = dpwRes.intelligentScores.acceptanceRatio,
                cancellationRatio = dpwRes.intelligentScores.cancellationRatio,
                driverAvailableTime = dpwRes.intelligentScores.availableTime,
                driverSpeed = dpwRes.intelligentScores.driverSpeed,
                keepHiddenForSeconds = dpwRes.keepHiddenForSeconds,
                mode = dpRes.mode,
                goHomeRequestId = dpwRes.goHomeReqId,
                ..
              }
      pure searchRequestForDriver

buildTranslatedSearchReqLocation :: (TranslateFlow m r, EsqDBFlow m r, CacheFlow m r) => DLoc.Location -> Maybe Maps.Language -> m DLoc.Location
buildTranslatedSearchReqLocation DLoc.Location {..} mbLanguage = do
  areaRegional <- case mbLanguage of
    Nothing -> return address.area
    Just lang -> do
      mAreaObj <- translate ENGLISH lang `mapM` address.area
      let translation = (\areaObj -> listToMaybe areaObj._data.translations) =<< mAreaObj
      return $ (.translatedText) <$> translation
  pure
    DLoc.Location
      { address =
          DLoc.LocationAddress
            { area = areaRegional,
              street = address.street,
              door = address.door,
              city = address.city,
              state = address.state,
              country = address.country,
              building = address.building,
              areaCode = address.areaCode,
              fullAddress = address.fullAddress
            },
        ..
      }

translateSearchReq ::
  ( TranslateFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DSR.SearchRequest ->
  Maps.Language ->
  m DSR.SearchRequest
translateSearchReq req@DSR.SearchRequest {..} language = do
  searchRequestDetails' <- case req.searchRequestDetails of
    DSR.SearchReqDetailsOnDemand DSR.SearchRequestDetailsOnDemand {..} -> do
      from <- buildTranslatedSearchReqLocation fromLocation (Just language)
      to <- buildTranslatedSearchReqLocation toLocation (Just language)
      pure $
        DSR.SearchReqDetailsOnDemand
          DSR.SearchRequestDetailsOnDemand
            { fromLocation = from,
              toLocation = to,
              ..
            }
    DSR.SearchReqDetailsRental DSR.SearchRequestDetailsRental {..} -> do
      from <- buildTranslatedSearchReqLocation rentalFromLocation (Just language)
      pure $
        DSR.SearchReqDetailsRental
          DSR.SearchRequestDetailsRental
            { rentalFromLocation = from,
              ..
            }
  pure $
    DSR.SearchRequest
      { searchRequestDetails = searchRequestDetails',
        ..
      }

addLanguageToDictionary ::
  ( TranslateFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  DSR.SearchRequest ->
  LanguageDictionary ->
  DriverPoolWithActualDistResult ->
  m LanguageDictionary
addLanguageToDictionary searchReq dict dPoolRes = do
  let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
  if isJust $ M.lookup language dict
    then return dict
    else do
      translatedSearchReq <- translateSearchReq searchReq language
      pure $ M.insert language translatedSearchReq dict
