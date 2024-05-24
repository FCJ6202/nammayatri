{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.RideBooking.Search where

import qualified API.UI.Search as SH
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()

data RideSearchEndPoint = SearchEndPoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "RideSearchEndPoint"

type API =
  "search"
    :> CustomerRideSearchAPI

type CustomerRideSearchAPI =
  Capture "customerId" (Id DP.Person)
    :> "rideSearch"
    :> ReqBody '[JSON] SH.SearchReq
    :> Post '[JSON] SH.SearchResp

handler :: ShortId DM.Merchant -> FlowServer API
handler = callSearch

callSearch :: ShortId DM.Merchant -> Id DP.Person -> SH.SearchReq -> FlowHandler SH.SearchResp
callSearch merchantId personId req = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  let req' = parseReq req
  SH.search (personId, m.id) req' Nothing Nothing Nothing Nothing Nothing (Just True)

parseReq :: SH.SearchReq -> SH.SearchReq
parseReq (SH.OneWaySearch oneWaySearchReq) = SH.OneWaySearch oneWaySearchReq
parseReq (SH.RentalSearch rentalSearchReq) = SH.RentalSearch rentalSearchReq
parseReq (SH.InterCitySearch interCitySearchReq) = SH.InterCitySearch interCitySearchReq
