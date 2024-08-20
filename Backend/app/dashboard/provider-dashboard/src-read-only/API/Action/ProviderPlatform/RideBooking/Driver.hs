{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.RideBooking.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.RideBooking.Driver
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Dashboard.ProviderPlatform.Fleet.Driver
import qualified Domain.Action.ProviderPlatform.RideBooking.Driver
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

type API = ("driver" :> (GetDriverPaymentDue :<|> PostDriverEnable :<|> PostDriverCollectCash :<|> PostDriverV2CollectCash :<|> PostDriverExemptCash :<|> PostDriverV2ExemptCash :<|> GetDriverInfo :<|> PostDriverUnlinkVehicle :<|> PostDriverEndRCAssociation :<|> PostDriverAddVehicle :<|> PostDriverSetRCStatus))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverPaymentDue merchantId city :<|> postDriverEnable merchantId city :<|> postDriverCollectCash merchantId city :<|> postDriverV2CollectCash merchantId city :<|> postDriverExemptCash merchantId city :<|> postDriverV2ExemptCash merchantId city :<|> getDriverInfo merchantId city :<|> postDriverUnlinkVehicle merchantId city :<|> postDriverEndRCAssociation merchantId city :<|> postDriverAddVehicle merchantId city :<|> postDriverSetRCStatus merchantId city

type GetDriverPaymentDue = (ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'BALANCE_DUE :> API.Types.ProviderPlatform.RideBooking.Driver.GetDriverPaymentDue)

type PostDriverEnable = (ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'ENABLE :> API.Types.ProviderPlatform.RideBooking.Driver.PostDriverEnable)

type PostDriverCollectCash = (ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'COLLECT_CASH :> API.Types.ProviderPlatform.RideBooking.Driver.PostDriverCollectCash)

type PostDriverV2CollectCash = (ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'COLLECT_CASH_V2 :> API.Types.ProviderPlatform.RideBooking.Driver.PostDriverV2CollectCash)

type PostDriverExemptCash = (ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'EXEMPT_CASH :> API.Types.ProviderPlatform.RideBooking.Driver.PostDriverExemptCash)

type PostDriverV2ExemptCash = (ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'EXEMPT_CASH_V2 :> API.Types.ProviderPlatform.RideBooking.Driver.PostDriverV2ExemptCash)

type GetDriverInfo = (ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'INFO :> API.Types.ProviderPlatform.RideBooking.Driver.GetDriverInfo)

type PostDriverUnlinkVehicle = (ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'UNLINK_VEHICLE :> API.Types.ProviderPlatform.RideBooking.Driver.PostDriverUnlinkVehicle)

type PostDriverEndRCAssociation = (ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'END_RC_ASSOCIATION :> API.Types.ProviderPlatform.RideBooking.Driver.PostDriverEndRCAssociation)

type PostDriverAddVehicle = (ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'ADD_VEHICLE :> API.Types.ProviderPlatform.RideBooking.Driver.PostDriverAddVehicle)

type PostDriverSetRCStatus = (ApiAuth 'DRIVER_OFFER_BPP 'DRIVERS 'SET_RC_STATUS :> API.Types.ProviderPlatform.RideBooking.Driver.PostDriverSetRCStatus)

getDriverPaymentDue :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.ProviderPlatform.RideBooking.Driver.DriverOutstandingBalanceResp])
getDriverPaymentDue merchantShortId opCity apiTokenInfo countryCode phone = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.getDriverPaymentDue merchantShortId opCity apiTokenInfo countryCode phone

postDriverEnable :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverEnable merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverEnable merchantShortId opCity apiTokenInfo driverId

postDriverCollectCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverCollectCash merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverCollectCash merchantShortId opCity apiTokenInfo driverId

postDriverV2CollectCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.Driver.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverV2CollectCash merchantShortId opCity apiTokenInfo driverId serviceName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverV2CollectCash merchantShortId opCity apiTokenInfo driverId serviceName

postDriverExemptCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverExemptCash merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverExemptCash merchantShortId opCity apiTokenInfo driverId

postDriverV2ExemptCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.Driver.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverV2ExemptCash merchantShortId opCity apiTokenInfo driverId serviceName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverV2ExemptCash merchantShortId opCity apiTokenInfo driverId serviceName

getDriverInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Environment.FlowHandler API.Types.ProviderPlatform.RideBooking.Driver.DriverInfoRes)
getDriverInfo merchantShortId opCity apiTokenInfo mobileNumber mobileCountryCode vehicleNumber dlNumber rcNumber email personId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.getDriverInfo merchantShortId opCity apiTokenInfo mobileNumber mobileCountryCode vehicleNumber dlNumber rcNumber email personId

postDriverUnlinkVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverUnlinkVehicle merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverUnlinkVehicle merchantShortId opCity apiTokenInfo driverId

postDriverEndRCAssociation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverEndRCAssociation merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverEndRCAssociation merchantShortId opCity apiTokenInfo driverId

postDriverAddVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.ProviderPlatform.Fleet.Driver.AddVehicleReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverAddVehicle merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverAddVehicle merchantShortId opCity apiTokenInfo driverId req

postDriverSetRCStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.ProviderPlatform.Fleet.Driver.RCStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSetRCStatus merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Driver.postDriverSetRCStatus merchantShortId opCity apiTokenInfo driverId req
