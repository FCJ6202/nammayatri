module Storage.Queries.FareBreakupExtra where

import Domain.Types.Booking
import Domain.Types.FareBreakup
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.FareBreakup as BeamFB
import Storage.Queries.OrphanInstances.FareBreakup ()

findAllByBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Booking -> m [FareBreakup]
findAllByBookingId bookingId = findAllWithKVAndConditionalDB [Se.Is BeamFB.bookingId $ Se.Eq $ getId bookingId] Nothing

deleteAllByBookingId :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> m ()
deleteAllByBookingId bookingId = deleteWithKV [Se.Is BeamFB.bookingId $ Se.Eq $ getId bookingId]
