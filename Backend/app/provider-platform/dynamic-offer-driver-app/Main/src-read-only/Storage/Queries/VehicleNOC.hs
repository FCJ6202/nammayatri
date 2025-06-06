{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleNOC where

import qualified Domain.Types.Image
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleNOC
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Documents
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleNOC as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleNOC.VehicleNOC -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleNOC.VehicleNOC] -> m ())
createMany = traverse_ create

findByImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.VehicleNOC.VehicleNOC))
findByImageId documentImageId = do findOneWithKV [Se.Is Beam.documentImageId $ Se.Eq (Kernel.Types.Id.getId documentImageId)]

findByRcIdAndDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.VehicleNOC.VehicleNOC])
findByRcIdAndDriverId rcId driverId = do findAllWithKV [Se.And [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId), Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateVerificationStatusByImageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatusByImageId verificationStatus documentImageId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.verificationStatus verificationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.documentImageId $ Se.Eq (Kernel.Types.Id.getId documentImageId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleNOC.VehicleNOC -> m (Maybe Domain.Types.VehicleNOC.VehicleNOC))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleNOC.VehicleNOC -> m ())
updateByPrimaryKey (Domain.Types.VehicleNOC.VehicleNOC {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.documentImageId (Kernel.Types.Id.getId documentImageId),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.nocExpiry nocExpiry,
      Se.Set Beam.nocNumberEncrypted (nocNumber & unEncrypted . encrypted),
      Se.Set Beam.nocNumberHash (nocNumber & hash),
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.verificationStatus verificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VehicleNOC Domain.Types.VehicleNOC.VehicleNOC where
  fromTType' (Beam.VehicleNOCT {..}) = do
    pure $
      Just
        Domain.Types.VehicleNOC.VehicleNOC
          { documentImageId = Kernel.Types.Id.Id documentImageId,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            nocExpiry = nocExpiry,
            nocNumber = EncryptedHashed (Encrypted nocNumberEncrypted) nocNumberHash,
            rcId = Kernel.Types.Id.Id rcId,
            verificationStatus = verificationStatus,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehicleNOC Domain.Types.VehicleNOC.VehicleNOC where
  toTType' (Domain.Types.VehicleNOC.VehicleNOC {..}) = do
    Beam.VehicleNOCT
      { Beam.documentImageId = Kernel.Types.Id.getId documentImageId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.nocExpiry = nocExpiry,
        Beam.nocNumberEncrypted = nocNumber & unEncrypted . encrypted,
        Beam.nocNumberHash = nocNumber & hash,
        Beam.rcId = Kernel.Types.Id.getId rcId,
        Beam.verificationStatus = verificationStatus,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
