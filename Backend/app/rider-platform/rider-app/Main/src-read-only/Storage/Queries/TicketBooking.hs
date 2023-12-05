{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketBooking where

import qualified Data.Time.Calendar as Data.Time.Calendar
import qualified Domain.Types.Merchant.MerchantOperatingCity as Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person as Domain.Types.Person
import qualified Domain.Types.TicketBooking as Domain.Types.TicketBooking
import qualified Domain.Types.TicketPlace as Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.Common as Kernel.Types.Common
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketBooking as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketBooking.TicketBooking -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.TicketBooking.TicketBooking] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking -> m (Maybe (Domain.Types.TicketBooking.TicketBooking))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByShortId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> m (Maybe (Domain.Types.TicketBooking.TicketBooking))
findByShortId (Kernel.Types.Id.ShortId shortId) = do
  findOneWithKV
    [ Se.Is Beam.shortId $ Se.Eq shortId
    ]

updateStatusByShortId :: MonadFlow m => Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Domain.Types.TicketBooking.BookingStatus -> m ()
updateStatusByShortId (Kernel.Types.Id.ShortId shortId) status = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.shortId $ Se.Eq shortId]

getAllBookingsByPersonId ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity ->
  Domain.Types.TicketBooking.BookingStatus ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.TicketBooking.TicketBooking]
getAllBookingsByPersonId personId merchantOpCityId status =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq personId.getId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId,
          Se.Is Beam.status $ Se.Eq status
        ]
    ]
    (Se.Desc Beam.createdAt)

instance FromTType' Beam.TicketBooking Domain.Types.TicketBooking.TicketBooking where
  fromTType' Beam.TicketBookingT {..} = do
    pure $
      Just
        Domain.Types.TicketBooking.TicketBooking
          { amount = amount,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            shortId = Kernel.Types.Id.ShortId shortId,
            status = status,
            ticketPlaceId = Kernel.Types.Id.Id ticketPlaceId,
            updatedAt = updatedAt,
            visitDate = visitDate
          }

instance ToTType' Beam.TicketBooking Domain.Types.TicketBooking.TicketBooking where
  toTType' Domain.Types.TicketBooking.TicketBooking {..} = do
    Beam.TicketBookingT
      { Beam.amount = amount,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.status = status,
        Beam.ticketPlaceId = Kernel.Types.Id.getId ticketPlaceId,
        Beam.updatedAt = updatedAt,
        Beam.visitDate = visitDate
      }
