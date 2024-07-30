{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.RideBooking.Driver
  ( getDriverPaymentDue,
    postDriverV2CollectCash,
    postDriverCollectCash,
    postDriverV2ExemptCash,
    postDriverExemptCash,
    postDriverEnable,
    getDriverInfo,
    postDriverUnlinkVehicle,
    postDriverEndRCAssociation,
    postDriverSetRCStatus,
    postDriverAddVehicle,
  )
where

import qualified Dashboard.ProviderPlatform.Fleet.Driver as Common
import qualified Dashboard.ProviderPlatform.RideBooking.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id

-- TODO move handlers from Domain.Action.Dashboard.Driver
getDriverPaymentDue ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Text ->
  Flow [Common.DriverOutstandingBalanceResp]
getDriverPaymentDue = DDriver.getDriverDue

postDriverEnable ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow APISuccess
postDriverEnable = DDriver.enableDriver

postDriverCollectCash ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Flow APISuccess
postDriverCollectCash = DDriver.collectCash

postDriverV2CollectCash ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Common.ServiceNames ->
  Flow APISuccess
postDriverV2CollectCash = DDriver.collectCashV2

postDriverExemptCash ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Flow APISuccess
postDriverExemptCash = DDriver.exemptCash

postDriverV2ExemptCash ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Common.ServiceNames ->
  Flow APISuccess
postDriverV2ExemptCash = DDriver.exemptCashV2

getDriverInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Bool ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.DriverInfoRes
getDriverInfo merchantShortId opCity fleetOwnerId mbFleet mobileNumber mobileCountryCode vehicleNumber dlNumber rcNumber email =
  DDriver.driverInfo merchantShortId opCity mobileNumber mobileCountryCode vehicleNumber dlNumber rcNumber email fleetOwnerId mbFleet

postDriverUnlinkVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow APISuccess
postDriverUnlinkVehicle = DDriver.unlinkVehicle

postDriverEndRCAssociation ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow APISuccess
postDriverEndRCAssociation = DDriver.endRCAssociation

postDriverAddVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.AddVehicleReq ->
  Flow APISuccess
postDriverAddVehicle = DDriver.addVehicle

postDriverSetRCStatus ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.RCStatusReq ->
  Flow APISuccess
postDriverSetRCStatus = DDriver.setRCStatus
