{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Init (API, handler) where

import qualified Beckn.ACL.Init as ACL
import qualified Beckn.ACL.OnInit as ACL
import qualified Beckn.Core as CallBAP
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.Prelude hiding (init)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Init.InitAPI

handler :: FlowServer API
handler = init

init ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  -- Init.InitReq ->
  ByteString ->
  FlowHandler AckResponse
init transporterId (SignatureAuthResult _ subscriber) reqBS = withFlowHandlerBecknAPI $ do
  req <- decodeReq reqBS
  case req of
    Left reqV1 ->
      withTransactionIdLogTag reqV1 $ do
        logTagInfo "Init API Flow" "Reached"
        dInitReq <- ACL.buildInitReqV1 subscriber reqV1
        Redis.whenWithLockRedis (initLockKey dInitReq.estimateId) 60 $ do
          let context = reqV1.context
          validatedRes <- DInit.validateRequest transporterId dInitReq
          fork "init request processing" $ do
            Redis.whenWithLockRedis (initProcessingLockKey dInitReq.estimateId) 60 $ do
              dInitRes <- DInit.handler transporterId dInitReq validatedRes
              void . handle (errHandler dInitRes.booking) $
                CallBAP.withCallback dInitRes.transporter Context.INIT OnInit.onInitAPI context context.bap_uri $
                  pure $ ACL.mkOnInitMessage dInitRes
        pure Ack
    Right reqV2 ->
      withTransactionIdLogTag reqV2 $ do
        logTagInfo "Init API Flow" "Reached"
        dInitReq <- ACL.buildInitReqV2 subscriber reqV2
        Redis.whenWithLockRedis (initLockKey dInitReq.estimateId) 60 $ do
          let context = reqV2.context
          validatedRes <- DInit.validateRequest transporterId dInitReq
          fork "init request processing" $ do
            Redis.whenWithLockRedis (initProcessingLockKey dInitReq.estimateId) 60 $ do
              dInitRes <- DInit.handler transporterId dInitReq validatedRes
              void . handle (errHandler dInitRes.booking) $
                CallBAP.withCallback dInitRes.transporter Context.INIT OnInit.onInitAPIV2 context context.bap_uri $
                  pure $ ACL.mkOnInitMessageV2 dInitRes
        pure Ack
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DInit.cancelBooking booking transporterId
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DInit.cancelBooking booking transporterId
      | otherwise = throwM exc

initLockKey :: Text -> Text
initLockKey id = "Driver:Init:DriverQuoteId-" <> id

initProcessingLockKey :: Text -> Text
initProcessingLockKey id = "Driver:Init:Processing:DriverQuoteId-" <> id

decodeReq :: MonadFlow m => ByteString -> m (Either Init.InitReq Init.InitReqV2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV1 -> pure $ Left reqV1
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV2 -> pure $ Right reqV2
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
