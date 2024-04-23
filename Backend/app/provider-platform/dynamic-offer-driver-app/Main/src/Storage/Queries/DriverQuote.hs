{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverQuote where

import qualified Data.Text as T
import qualified Data.Time as T
import qualified Domain.Types.Common as DTC
import Domain.Types.DriverQuote
import qualified Domain.Types.DriverQuote as Domain
import qualified Domain.Types.Estimate as DEstimate
import Domain.Types.Person
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Sequelize as Se
import SharedLogic.DriverPool.Types
import qualified Storage.Beam.DriverQuote as BeamDQ
import Storage.Queries.FareParameters as BeamQFP
import qualified Storage.Queries.FareParameters as SQFP

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.DriverQuote -> m ()
create dQuote = SQFP.create dQuote.fareParams >> createWithKV dQuote

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.DriverQuote -> m (Maybe Domain.DriverQuote)
findById (Id driverQuoteId) = findOneWithKV [Se.Is BeamDQ.id $ Se.Eq driverQuoteId]

setInactiveBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DST.SearchTry -> m ()
setInactiveBySTId (Id searchTryId) = updateWithKV [Se.Set BeamDQ.status Domain.Inactive] [Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId]

setInactiveBySRId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DSR.SearchRequest -> m ()
setInactiveBySRId (Id searchReqId) = updateWithKV [Se.Set BeamDQ.status Domain.Inactive] [Se.Is BeamDQ.requestId $ Se.Eq searchReqId]

findActiveQuotesByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Seconds -> m [Domain.DriverQuote]
findActiveQuotesByDriverId (Id driverId) driverUnlockDelay = do
  now <- getCurrentTime
  let delayToAvoidRaces = secondsToNominalDiffTime . negate $ driverUnlockDelay
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamDQ.status $ Se.Eq Domain.Active,
          Se.Is BeamDQ.driverId $ Se.Eq driverId,
          Se.Is BeamDQ.validTill $ Se.GreaterThan (T.utcToLocalTime T.utc $ addUTCTime delayToAvoidRaces now)
        ]
    ]
    Nothing

findDriverQuoteBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DST.SearchTry -> m (Maybe Domain.DriverQuote)
findDriverQuoteBySTId (Id searchTryId) = findOneWithKV [Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId]

deleteByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteByDriverId (Id driverId) = deleteWithKV [Se.Is BeamDQ.driverId (Se.Eq driverId)]

findAllBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DST.SearchTry -> m [Domain.DriverQuote]
findAllBySTId (Id searchTryId) =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId,
          Se.Is BeamDQ.status $ Se.Eq Domain.Active
        ]
    ]
    Nothing

countAllBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DST.SearchTry -> m Int
countAllBySTId searchTId =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is BeamDQ.searchTryId $ Se.Eq (getId searchTId),
          Se.Is BeamDQ.status $ Se.Eq Domain.Active
        ]
    ]
    Nothing
    <&> length

setInactiveAllDQByEstId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DEstimate.Estimate -> UTCTime -> m ()
setInactiveAllDQByEstId (Id estimateId) now = updateWithKV [Se.Set BeamDQ.status Domain.Inactive, Se.Set BeamDQ.updatedAt (T.utcToLocalTime T.utc now)] [Se.And [Se.Is BeamDQ.estimateId $ Se.Eq estimateId, Se.Is BeamDQ.status $ Se.Eq Domain.Active, Se.Is BeamDQ.validTill $ Se.GreaterThan (T.utcToLocalTime T.utc now)]]

instance FromTType' BeamDQ.DriverQuote DriverQuote where
  fromTType' BeamDQ.DriverQuoteT {..} = do
    fp <- BeamQFP.findById (Id fareParametersId) >>= fromMaybeM (InternalError $ "FareParameters not found in DriverQuote for id: " <> show fareParametersId)
    clientSdkVersion' <- mapM readVersion (T.strip <$> clientSdkVersion)
    clientBundleVersion' <- mapM readVersion (T.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM readVersion (T.strip <$> clientConfigVersion)
    backendConfigVersion' <- mapM readVersion (T.strip <$> backendConfigVersion)
    let clientDevice' = mkClientDevice clientOsType clientOsVersion
    return $
      Just
        Domain.DriverQuote
          { id = Id id,
            requestId = Id requestId,
            searchTryId = Id searchTryId,
            clientId = Id <$> clientId,
            searchRequestForDriverId = Id <$> searchRequestForDriverId,
            tripCategory = fromMaybe (DTC.OneWay DTC.OneWayOnDemandDynamicOffer) tripCategory,
            driverId = Id driverId,
            estimateId = Id estimateId,
            driverName = driverName,
            driverRating = driverRating,
            status = status,
            vehicleVariant = vehicleVariant,
            vehicleServiceTier = fromMaybe (castVariantToServiceTier vehicleVariant) vehicleServiceTier,
            distance = distance,
            distanceToPickup = distanceToPickup,
            durationToPickup = durationToPickup,
            createdAt = T.localTimeToUTC T.utc createdAt,
            updatedAt = T.localTimeToUTC T.utc updatedAt,
            validTill = T.localTimeToUTC T.utc validTill,
            estimatedFare = estimatedFare,
            fareParams = fp,
            providerId = Id providerId,
            goHomeRequestId = Id <$> goHomeRequestId,
            specialLocationTag = specialLocationTag,
            clientSdkVersion = clientSdkVersion',
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            backendConfigVersion = backendConfigVersion',
            clientDevice = clientDevice',
            ..
          }

instance ToTType' BeamDQ.DriverQuote DriverQuote where
  toTType' DriverQuote {..} = do
    BeamDQ.DriverQuoteT
      { BeamDQ.id = getId id,
        BeamDQ.requestId = getId requestId,
        BeamDQ.searchTryId = getId searchTryId,
        BeamDQ.clientId = getId <$> clientId,
        BeamDQ.searchRequestForDriverId = getId <$> searchRequestForDriverId,
        BeamDQ.driverId = getId driverId,
        BeamDQ.estimateId = getId estimateId,
        BeamDQ.tripCategory = Just tripCategory,
        BeamDQ.driverName = driverName,
        BeamDQ.driverRating = driverRating,
        BeamDQ.status = status,
        BeamDQ.vehicleVariant = vehicleVariant,
        BeamDQ.vehicleServiceTier = Just vehicleServiceTier,
        BeamDQ.distance = distance,
        BeamDQ.distanceToPickup = distanceToPickup,
        BeamDQ.durationToPickup = durationToPickup,
        BeamDQ.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamDQ.updatedAt = T.utcToLocalTime T.utc updatedAt,
        BeamDQ.validTill = T.utcToLocalTime T.utc validTill,
        BeamDQ.estimatedFare = estimatedFare,
        BeamDQ.fareParametersId = getId fareParams.id,
        BeamDQ.providerId = getId providerId,
        BeamDQ.goHomeRequestId = getId <$> goHomeRequestId,
        BeamDQ.specialLocationTag = specialLocationTag,
        BeamDQ.clientSdkVersion = versionToText <$> clientSdkVersion,
        BeamDQ.clientBundleVersion = versionToText <$> clientBundleVersion,
        BeamDQ.clientConfigVersion = versionToText <$> clientConfigVersion,
        BeamDQ.backendConfigVersion = versionToText <$> backendConfigVersion,
        BeamDQ.clientOsVersion = clientDevice <&> (.deviceVersion),
        BeamDQ.clientOsType = clientDevice <&> (.deviceType),
        ..
      }
