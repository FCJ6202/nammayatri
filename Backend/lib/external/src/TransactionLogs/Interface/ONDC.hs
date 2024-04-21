{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module TransactionLogs.Interface.ONDC where

import qualified Data.Aeson as A
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import qualified TransactionLogs.Interface.Types as IT
import qualified TransactionLogs.ONDC.Flow as OF
import TransactionLogs.ONDC.Types as ONDC

pushTxnLogs ::
  (CoreMetrics m, MonadFlow m) =>
  ONDCConfig ->
  IT.TransactionLogReq ->
  m ()
pushTxnLogs config req =
  OF.pushTxnLogsAPI config (mkONDCRequest req)

mkONDCRequest :: IT.TransactionLogReq -> ONDC.ONDCRequest
mkONDCRequest IT.TransactionLogReq {..} = do
  let jsonText = LT.toStrict $ LTE.decodeUtf8 $ A.encode logData
  ONDC.ONDCRequest
    { _type = logType,
      _data = jsonText
    }
