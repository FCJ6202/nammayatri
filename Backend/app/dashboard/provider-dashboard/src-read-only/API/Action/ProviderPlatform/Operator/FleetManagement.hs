{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Operator.FleetManagement
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator
import qualified API.Types.ProviderPlatform.Operator.FleetManagement
import qualified Domain.Action.ProviderPlatform.Operator.FleetManagement
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("operator" :> GetFleetManagementFleets)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFleetManagementFleets merchantId city

type GetFleetManagementFleets =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_OPERATOR) / ('API.Types.ProviderPlatform.Operator.FLEET_MANAGEMENT) / ('API.Types.ProviderPlatform.Operator.FleetManagement.GET_FLEET_MANAGEMENT_FLEETS))
      :> API.Types.ProviderPlatform.Operator.FleetManagement.GetFleetManagementFleets
  )

getFleetManagementFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler [API.Types.ProviderPlatform.Operator.FleetManagement.FleetInfo])
getFleetManagementFleets merchantShortId opCity apiTokenInfo isActive verified limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.FleetManagement.getFleetManagementFleets merchantShortId opCity apiTokenInfo isActive verified limit offset
