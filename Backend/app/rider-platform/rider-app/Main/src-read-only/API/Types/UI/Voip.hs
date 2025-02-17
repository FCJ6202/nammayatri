{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Voip where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data UserType
  = DRIVER
  | RIDER
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VoipReq = VoipReq
  { callId :: Data.Text.Text,
    callStatus :: VoipStatus,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    userType :: UserType,
    networkType :: Data.Text.Text,
    networkQuality :: Data.Text.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VoipStatus
  = CALL_IS_PLACED
  | CALL_RINGING
  | CALL_MISSED
  | CALL_CANCELLED
  | CALL_DECLINED_DUE_TO_BUSY_ON_PSTN
  | CALL_DECLINED_DUE_TO_BUSY_ON_VOIP
  | CALL_OVER_DUE_TO_NETWORK_DELAY_IN_MEDIA_SETUP
  | CALL_OVER_DUE_TO_PROTOCOL_MISMATCH
  | CALL_OVER_DUE_TO_REMOTE_NETWORK_LOSS
  | CALL_OVER_DUE_TO_LOCAL_NETWORK_LOSS
  | CALL_IN_PROGRESS
  | CALL_ANSWERED
  | CALL_CANCELLED_DUE_TO_RING_TIMEOUT
  | CALL_DECLINED
  | CALL_OVER
  | CALL_DECLINED_DUE_TO_LOGGED_OUT_CUID
  | CALL_DECLINED_DUE_TO_NOTIFICATIONS_DISABLED
  | CALLEE_MICROPHONE_PERMISSION_BLOCKED
  | CALL_FAILED_DUE_TO_INTERNAL_ERROR
  | MULTIPLE_VOIP_CALL_ATTEMPTS
  | MIC_PERMISSION_DENIED
  | NETWORK_ERROR
  | NO_INTERNET
  | SDK_NOT_INIT
  | UNKNOWN_ERROR
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
