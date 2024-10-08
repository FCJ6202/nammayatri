{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Journey where

import qualified Domain.Types.Journey
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Journey as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Journey.Journey -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Journey.Journey] -> m ())
createMany = traverse_ create

updateEstimatedFare :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.Price -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m ())
updateEstimatedFare estimatedFare id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.estimatedFare (Kernel.Prelude.fmap (.amount) estimatedFare), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateNumberOfLegs :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m ())
updateNumberOfLegs legsDone id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.legsDone legsDone, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m (Maybe Domain.Types.Journey.Journey))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Journey.Journey -> m ())
updateByPrimaryKey (Domain.Types.Journey.Journey {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.convenienceCost convenienceCost,
      Se.Set Beam.distanceUnit ((.unit) estimatedDistance),
      Se.Set Beam.estimatedDistance ((.value) estimatedDistance),
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.estimatedFare (Kernel.Prelude.fmap (.amount) estimatedFare),
      Se.Set Beam.currency (Kernel.Prelude.fmap (.currency) fare),
      Se.Set Beam.fare (Kernel.Prelude.fmap (.amount) fare),
      Se.Set Beam.legsDone legsDone,
      Se.Set Beam.modes modes,
      Se.Set Beam.searchRequestId (Kernel.Types.Id.getId searchRequestId),
      Se.Set Beam.totalLegs totalLegs,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Journey Domain.Types.Journey.Journey where
  fromTType' (Beam.JourneyT {..}) = do
    pure $
      Just
        Domain.Types.Journey.Journey
          { convenienceCost = convenienceCost,
            estimatedDistance = Kernel.Types.Common.Distance estimatedDistance distanceUnit,
            estimatedDuration = estimatedDuration,
            estimatedFare = Kernel.Types.Common.mkPrice currency <$> estimatedFare,
            fare = Kernel.Types.Common.mkPrice currency <$> fare,
            id = Kernel.Types.Id.Id id,
            legsDone = legsDone,
            modes = modes,
            searchRequestId = Kernel.Types.Id.Id searchRequestId,
            totalLegs = totalLegs,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Journey Domain.Types.Journey.Journey where
  toTType' (Domain.Types.Journey.Journey {..}) = do
    Beam.JourneyT
      { Beam.convenienceCost = convenienceCost,
        Beam.distanceUnit = (.unit) estimatedDistance,
        Beam.estimatedDistance = (.value) estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.estimatedFare = Kernel.Prelude.fmap (.amount) estimatedFare,
        Beam.currency = Kernel.Prelude.fmap (.currency) fare,
        Beam.fare = Kernel.Prelude.fmap (.amount) fare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.legsDone = legsDone,
        Beam.modes = modes,
        Beam.searchRequestId = Kernel.Types.Id.getId searchRequestId,
        Beam.totalLegs = totalLegs,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
