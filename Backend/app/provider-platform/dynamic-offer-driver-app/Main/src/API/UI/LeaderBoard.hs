module API.UI.LeaderBoard where

import Data.Time
import qualified Domain.Action.UI.LeaderBoard as DLeaderBoard
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "driver" :> "leaderBoard"
    :> ( TokenAuth
           :> "daily"
           :> MandatoryQueryParam "date" Day
           :> Get '[JSON] DLeaderBoard.LeaderBoardRes
           :<|> TokenAuth
           :> "weekly"
           :> MandatoryQueryParam "fromDate" Day
           :> MandatoryQueryParam "toDate" Day
           :> Get '[JSON] DLeaderBoard.LeaderBoardRes
       )

handler :: FlowServer API
handler =
  getDailyDriverLeaderBoard
    :<|> getWeeklyDriverLeaderBoard

getDailyDriverLeaderBoard :: (Id SP.Person, Id DM.Merchant) -> Day -> FlowHandler DLeaderBoard.LeaderBoardRes
getDailyDriverLeaderBoard (personId, merchantId) date = withFlowHandlerAPI $ DLeaderBoard.getDailyDriverLeaderBoard (personId, merchantId) date

getWeeklyDriverLeaderBoard :: (Id SP.Person, Id DM.Merchant) -> Day -> Day -> FlowHandler DLeaderBoard.LeaderBoardRes
getWeeklyDriverLeaderBoard (personId, merchantId) fromDate toDate = withFlowHandlerAPI $ DLeaderBoard.getWeeklyDriverLeaderBoard (personId, merchantId) fromDate toDate
