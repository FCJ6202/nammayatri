{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOnboarding.DriverRCAssociation where

import qualified Data.HashMap.Strict as HashMap
import Domain.Types.DriverOnboarding.DriverRCAssociation as DRCA
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Person (Person)
import qualified EulerHS.Language as L
import Kernel.Prelude hiding (on)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, deleteWithKV, findAllWithKV, findAllWithKvInReplica, findAllWithOptionsKV, findAllWithOptionsKvInReplica, findOneWithKV, updateWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.DriverRCAssociation as BeamDRCA
import qualified Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as BeamVRC
import Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate ()

create :: (L.MonadFlow m, Log m) => DriverRCAssociation -> m ()
create driverRCAssociation = createWithKV driverRCAssociation

findById :: (L.MonadFlow m, Log m) => Id DriverRCAssociation -> m (Maybe DriverRCAssociation)
findById (Id drcaId) = findOneWithKV [Se.Is BeamDRCA.id $ Se.Eq drcaId]

getActiveAssociationByDriver :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> m (Maybe DriverRCAssociation)
getActiveAssociationByDriver (Id personId) = do
  now <- getCurrentTime
  findOneWithKV [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq personId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]

-- findAllByDriverId ::
--   Transactionable m =>
--   Id Person ->
--   m [(DriverRCAssociation, VehicleRegistrationCertificate)]
-- findAllByDriverId driverId = do
--   rcAssocs <- getRcAssocs driverId
--   regCerts <- getRegCerts rcAssocs
--   return $ linkDriversRC rcAssocs regCerts

findAllByDriverId ::
  (L.MonadFlow m, Log m) =>
  Id Person ->
  m [(DriverRCAssociation, VehicleRegistrationCertificate)]
findAllByDriverId driverId = do
  rcAssocs <- getRcAssocs driverId
  regCerts <- getRegCerts rcAssocs
  return $ linkDriversRC rcAssocs regCerts

findAllByDriverIdInReplica ::
  (L.MonadFlow m, Log m) =>
  Id Person ->
  m [(DriverRCAssociation, VehicleRegistrationCertificate)]
findAllByDriverIdInReplica driverId = do
  rcAssocs <- getRcAssocsInReplica driverId
  regCerts <- getRegCertsInReplica rcAssocs
  return $ linkDriversRC rcAssocs regCerts

linkDriversRC :: [DriverRCAssociation] -> [VehicleRegistrationCertificate] -> [(DriverRCAssociation, VehicleRegistrationCertificate)]
linkDriversRC rcAssocs regCerts = do
  let certHM = buildCertHM regCerts
   in mapMaybe (mapRCWithDriver certHM) rcAssocs

mapRCWithDriver :: HashMap.HashMap Text VehicleRegistrationCertificate -> DriverRCAssociation -> Maybe (DriverRCAssociation, VehicleRegistrationCertificate)
mapRCWithDriver certHM rcAssoc = do
  let rcId = rcAssoc.rcId.getId
  cert <- HashMap.lookup rcId certHM
  Just (rcAssoc, cert)

buildRcHM :: [DriverRCAssociation] -> HashMap.HashMap Text DriverRCAssociation
buildRcHM rcAssocs =
  HashMap.fromList $ map (\r -> (r.rcId.getId, r)) rcAssocs

buildCertHM :: [VehicleRegistrationCertificate] -> HashMap.HashMap Text VehicleRegistrationCertificate
buildCertHM regCerts =
  HashMap.fromList $ map (\r -> (r.id.getId, r)) regCerts

-- getRegCerts ::
--   Transactionable m =>
--   [DriverRCAssociation] ->
--   m [VehicleRegistrationCertificate]
-- getRegCerts rcAssocs = do
--   Esq.findAll $ do
--     regCerts <- from $ table @VehicleRegistrationCertificateT
--     return regCerts
--   where
--     rcAssocsKeys = toKey . cast <$> fetchRcIdFromAssocs rcAssocs

getRegCerts :: (L.MonadFlow m, Log m) => [DriverRCAssociation] -> m [VehicleRegistrationCertificate]
getRegCerts rcAssocs = findAllWithKV [Se.Is BeamVRC.id $ Se.In $ getId <$> fetchRcIdFromAssocs rcAssocs]

getRegCertsInReplica :: (L.MonadFlow m, Log m) => [DriverRCAssociation] -> m [VehicleRegistrationCertificate]
getRegCertsInReplica rcAssocs = findAllWithKvInReplica [Se.Is BeamVRC.id $ Se.In $ getId <$> fetchRcIdFromAssocs rcAssocs]

fetchRcIdFromAssocs :: [DriverRCAssociation] -> [Id VehicleRegistrationCertificate]
fetchRcIdFromAssocs = map (.rcId)

-- getRcAssocs ::
--   Transactionable m =>
--   Id Person ->
--   m [DriverRCAssociation]
-- getRcAssocs driverId = do
--   Esq.findAll $ do
--     rcAssoc <- from $ table @DriverRCAssociationT
--     where_ $
--       rcAssoc ^. DriverRCAssociationDriverId ==. val (toKey driverId)
--     orderBy [desc $ rcAssoc ^. DriverRCAssociationAssociatedOn]
--     return rcAssoc

getRcAssocs :: (L.MonadFlow m, Log m) => Id Person -> m [DriverRCAssociation]
getRcAssocs (Id driverId) = findAllWithOptionsKV [Se.Is BeamDRCA.driverId $ Se.Eq driverId] (Se.Desc BeamDRCA.associatedOn) Nothing Nothing

getRcAssocsInReplica :: (L.MonadFlow m, Log m) => Id Person -> m [DriverRCAssociation]
getRcAssocsInReplica (Id driverId) = findAllWithOptionsKvInReplica [Se.Is BeamDRCA.driverId $ Se.Eq driverId] (Se.Desc BeamDRCA.associatedOn) Nothing Nothing

getActiveAssociationByRC :: (L.MonadFlow m, MonadTime m, Log m) => Id VehicleRegistrationCertificate -> m (Maybe DriverRCAssociation)
getActiveAssociationByRC (Id rcId) = do
  now <- getCurrentTime
  findOneWithKV [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]

endAssociation :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> m ()
endAssociation (Id driverId) = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamDRCA.associatedTill $ Just now]
    [Se.And [Se.Is BeamDRCA.id (Se.Eq driverId), Se.Is BeamDRCA.associatedTill (Se.GreaterThan $ Just now)]]

deleteByDriverId :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteByDriverId (Id driverId) = deleteWithKV [Se.Is BeamDRCA.driverId (Se.Eq driverId)]

instance FromTType' BeamDRCA.DriverRCAssociation DriverRCAssociation where
  fromTType' BeamDRCA.DriverRCAssociationT {..} = do
    pure $
      Just
        DriverRCAssociation
          { id = Id id,
            driverId = Id driverId,
            rcId = Id rcId,
            associatedOn = associatedOn,
            associatedTill = associatedTill,
            consent = consent,
            consentTimestamp = consentTimestamp
          }

instance ToTType' BeamDRCA.DriverRCAssociation DriverRCAssociation where
  toTType' DriverRCAssociation {..} = do
    BeamDRCA.DriverRCAssociationT
      { BeamDRCA.id = getId id,
        BeamDRCA.driverId = getId driverId,
        BeamDRCA.rcId = getId rcId,
        BeamDRCA.associatedOn = associatedOn,
        BeamDRCA.associatedTill = associatedTill,
        BeamDRCA.consent = consent,
        BeamDRCA.consentTimestamp = consentTimestamp
      }
