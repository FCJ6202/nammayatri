{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.BookingCancellationReason where

import Domain.Types.Booking
import Domain.Types.BookingCancellationReason
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import Domain.Types.Ride
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.BookingCancellationReason as BeamBCR
import Storage.Tabular.BookingCancellationReason

create :: BookingCancellationReason -> SqlDB ()
create = Esq.create

findByRideBookingId ::
  Transactionable m =>
  Id Booking ->
  m (Maybe BookingCancellationReason)
findByRideBookingId rideBookingId =
  Esq.findOne $ do
    rideBookingCancellationReason <- from $ table @BookingCancellationReasonT
    where_ $ rideBookingCancellationReason ^. BookingCancellationReasonBookingId ==. val (toKey rideBookingId)
    return rideBookingCancellationReason

findByRideId :: Transactionable m => Id Ride -> m (Maybe BookingCancellationReason)
findByRideId rideId = Esq.findOne $ do
  bookingCancellationReason <- from $ table @BookingCancellationReasonT
  where_ $ bookingCancellationReason ^. BookingCancellationReasonRideId ==. (just . val . toKey $ rideId)
  return bookingCancellationReason

upsert :: BookingCancellationReason -> SqlDB ()
upsert cancellationReason =
  Esq.upsert
    cancellationReason
    [ BookingCancellationReasonBookingId =. val (toKey cancellationReason.bookingId),
      BookingCancellationReasonRideId =. val (toKey <$> cancellationReason.rideId),
      BookingCancellationReasonReasonCode =. val (toKey <$> cancellationReason.reasonCode),
      BookingCancellationReasonAdditionalInfo =. val (cancellationReason.additionalInfo)
    ]

transformBeamBookingCancellationReasonToDomain :: BeamBCR.BookingCancellationReason -> BookingCancellationReason
transformBeamBookingCancellationReasonToDomain BeamBCR.BookingCancellationReasonT {..} = do
  BookingCancellationReason
    { driverId = Id <$> driverId,
      bookingId = Id bookingId,
      rideId = Id <$> rideId,
      source = source,
      reasonCode = CancellationReasonCode <$> reasonCode,
      additionalInfo = additionalInfo
    }

transformDomainBookingCancellationReasonToBeam :: BookingCancellationReason -> BeamBCR.BookingCancellationReason
transformDomainBookingCancellationReasonToBeam BookingCancellationReason {..} =
  BeamBCR.defaultBookingCancellationReason
    { BeamBCR.driverId = getId <$> driverId,
      BeamBCR.bookingId = getId bookingId,
      BeamBCR.rideId = getId <$> rideId,
      BeamBCR.source = source,
      BeamBCR.reasonCode = (\(CancellationReasonCode x) -> x) <$> reasonCode,
      BeamBCR.additionalInfo = additionalInfo
    }
