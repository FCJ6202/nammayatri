{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FareParameters where

import Domain.Types.FareParameters
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.FareParameters as BeamFP
import Storage.Tabular.FareParameters ()

create :: FareParameters -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id FareParameters -> m (Maybe FareParameters)
findById = Esq.findById

transformBeamFareParametersToDomain :: BeamFP.FareParameters -> FareParameters
transformBeamFareParametersToDomain BeamFP.FareParametersT {..} = do
  FareParameters
    { id = Id id,
      baseFare = baseFare,
      deadKmFare = deadKmFare,
      extraKmFare = extraKmFare,
      driverSelectedFare = driverSelectedFare,
      customerExtraFee = customerExtraFee,
      nightShiftRate = nightShiftRate,
      nightCoefIncluded = nightCoefIncluded,
      waitingChargePerMin = waitingChargePerMin,
      waitingOrPickupCharges = waitingOrPickupCharges,
      serviceCharge = serviceCharge,
      farePolicyType = farePolicyType,
      govtChargesPerc = govtChargesPerc
    }

transformDomainFareParametersToBeam :: FareParameters -> BeamFP.FareParameters
transformDomainFareParametersToBeam FareParameters {..} =
  BeamFP.defaultFareParameters
    { BeamFP.id = getId id,
      BeamFP.baseFare = baseFare,
      BeamFP.deadKmFare = deadKmFare,
      BeamFP.extraKmFare = extraKmFare,
      BeamFP.driverSelectedFare = driverSelectedFare,
      BeamFP.customerExtraFee = customerExtraFee,
      BeamFP.nightShiftRate = nightShiftRate,
      BeamFP.nightCoefIncluded = nightCoefIncluded,
      BeamFP.waitingChargePerMin = waitingChargePerMin,
      BeamFP.waitingOrPickupCharges = waitingOrPickupCharges,
      BeamFP.serviceCharge = serviceCharge,
      BeamFP.farePolicyType = farePolicyType,
      BeamFP.govtChargesPerc = govtChargesPerc
    }
