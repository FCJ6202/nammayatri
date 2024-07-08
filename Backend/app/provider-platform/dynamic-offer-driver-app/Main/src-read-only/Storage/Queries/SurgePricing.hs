{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SurgePricing where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SurgePricing
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SurgePricing as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SurgePricing.SurgePricing -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SurgePricing.SurgePricing] -> m ())
createMany = traverse_ create

findByHexDayHourForMerchantOpCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Int -> m (Maybe Domain.Types.SurgePricing.SurgePricing))
findByHexDayHourForMerchantOpCity merchantOperatingCityId sourceHex dayOfWeek hourOfDay = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.sourceHex $ Se.Eq sourceHex,
          Se.Is Beam.dayOfWeek $ Se.Eq dayOfWeek,
          Se.Is Beam.hourOfDay $ Se.Eq hourOfDay
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SurgePricing.SurgePricing -> m (Maybe Domain.Types.SurgePricing.SurgePricing))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SurgePricing.SurgePricing -> m ())
updateByPrimaryKey (Domain.Types.SurgePricing.SurgePricing {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.dayOfWeek dayOfWeek,
      Se.Set Beam.hourOfDay hourOfDay,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.sourceHex sourceHex,
      Se.Set Beam.surgeMultiplier surgeMultiplier,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SurgePricing Domain.Types.SurgePricing.SurgePricing where
  fromTType' (Beam.SurgePricingT {..}) = do
    pure $
      Just
        Domain.Types.SurgePricing.SurgePricing
          { dayOfWeek = dayOfWeek,
            hourOfDay = hourOfDay,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            sourceHex = sourceHex,
            surgeMultiplier = surgeMultiplier,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SurgePricing Domain.Types.SurgePricing.SurgePricing where
  toTType' (Domain.Types.SurgePricing.SurgePricing {..}) = do
    Beam.SurgePricingT
      { Beam.dayOfWeek = dayOfWeek,
        Beam.hourOfDay = hourOfDay,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.sourceHex = sourceHex,
        Beam.surgeMultiplier = surgeMultiplier,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
