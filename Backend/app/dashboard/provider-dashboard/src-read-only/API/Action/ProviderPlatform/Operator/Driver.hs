{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Operator.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator
import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified Domain.Action.ProviderPlatform.Operator.Driver
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driver" :> (GetDriverOperatorFetchHubRequests :<|> GetDriverOperationGetAllHubs :<|> PostDriverOperatorRespondHubRequest :<|> PostDriverOperatorCreateRequest :<|> GetDriverOperatorList))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverOperatorFetchHubRequests merchantId city :<|> getDriverOperationGetAllHubs merchantId city :<|> postDriverOperatorRespondHubRequest merchantId city :<|> postDriverOperatorCreateRequest merchantId city :<|> getDriverOperatorList merchantId city

type GetDriverOperatorFetchHubRequests =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.DRIVER / 'API.Types.ProviderPlatform.Operator.Driver.GET_DRIVER_OPERATOR_FETCH_HUB_REQUESTS)
      :> API.Types.ProviderPlatform.Operator.Driver.GetDriverOperatorFetchHubRequests
  )

type GetDriverOperationGetAllHubs =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.DRIVER / 'API.Types.ProviderPlatform.Operator.Driver.GET_DRIVER_OPERATION_GET_ALL_HUBS)
      :> API.Types.ProviderPlatform.Operator.Driver.GetDriverOperationGetAllHubs
  )

type PostDriverOperatorRespondHubRequest =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.DRIVER / 'API.Types.ProviderPlatform.Operator.Driver.POST_DRIVER_OPERATOR_RESPOND_HUB_REQUEST)
      :> API.Types.ProviderPlatform.Operator.Driver.PostDriverOperatorRespondHubRequest
  )

type PostDriverOperatorCreateRequest =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.DRIVER / 'API.Types.ProviderPlatform.Operator.Driver.POST_DRIVER_OPERATOR_CREATE_REQUEST)
      :> API.Types.ProviderPlatform.Operator.Driver.PostDriverOperatorCreateRequest
  )

type GetDriverOperatorList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.DRIVER / 'API.Types.ProviderPlatform.Operator.Driver.GET_DRIVER_OPERATOR_LIST)
      :> API.Types.ProviderPlatform.Operator.Driver.GetDriverOperatorList
  )

getDriverOperatorFetchHubRequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Operator.Driver.RequestStatus -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Operator.Driver.RequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id API.Types.ProviderPlatform.Operator.Driver.OperationHub) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp)
getDriverOperatorFetchHubRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbStatus mbReqType mbLimit mbOffset mbDriverId mbMobileNumber mbOperationHubId mbRegistrationNo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Driver.getDriverOperatorFetchHubRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbStatus mbReqType mbLimit mbOffset mbDriverId mbMobileNumber mbOperationHubId mbRegistrationNo

getDriverOperationGetAllHubs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler [API.Types.ProviderPlatform.Operator.Driver.OperationHub])
getDriverOperationGetAllHubs merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Driver.getDriverOperationGetAllHubs merchantShortId opCity apiTokenInfo

postDriverOperatorRespondHubRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.Driver.RespondHubRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorRespondHubRequest merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Driver.postDriverOperatorRespondHubRequest merchantShortId opCity apiTokenInfo req

postDriverOperatorCreateRequest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverOperatorCreateRequest merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Driver.postDriverOperatorCreateRequest merchantShortId opCity apiTokenInfo req

getDriverOperatorList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler [API.Types.ProviderPlatform.Operator.Driver.DriverInfo])
getDriverOperatorList merchantShortId opCity apiTokenInfo isActive limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Driver.getDriverOperatorList merchantShortId opCity apiTokenInfo isActive limit offset
