module Domain.Action.UI.Performance where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Domain.Types.RiderDetails ()
import qualified Domain.Types.Vehicle as DV
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBFlow, EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, fromMaybeM)
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error

data Results = Results
  { totalReferredCustomers :: Int,
    totalActivatedCustomers :: Int,
    totalReferredDrivers :: Int,
    isPayoutEnabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype PerformanceRes = PerformanceRes
  { referrals :: Results
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

getDriverPerformance :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> m PerformanceRes
getDriverPerformance (driverId, _, merchantOpCityId) = do
  _ <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  allRefferedCustomers <- QRD.findAllReferredByDriverId (Just driverId)
  let ridesTakenList = filter (.hasTakenValidRide) allRefferedCustomers
  di <- B.runInReplica (DriverInformation.findById driverId) >>= fromMaybeM DriverInfoNotFound
  let totalReferredDrivers = fromMaybe 0 di.totalReferred
  mbVehicle <- QVeh.findById driverId
  let vehicleCategory = fromMaybe DV.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicleCategory >>= fromMaybeM (InternalError "Payout config not present")
  pure $ PerformanceRes (Results (length allRefferedCustomers) (length ridesTakenList) totalReferredDrivers payoutConfig.isPayoutEnabled)
