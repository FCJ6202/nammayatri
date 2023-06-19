{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Payment.PaymentOrder where

import qualified Domain.Types.Payment.PaymentOrder as DOrder
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
import Storage.Tabular.Payment.PaymentOrder

findById :: Transactionable m => Id DOrder.PaymentOrder -> m (Maybe DOrder.PaymentOrder)
findById = Esq.findById

findByShortId :: Transactionable m => ShortId DOrder.PaymentOrder -> m (Maybe DOrder.PaymentOrder)
findByShortId shortId =
  findOne $ do
    order <- from $ table @PaymentOrderT
    where_ $ order ^. PaymentOrderShortId ==. val (getShortId shortId)
    return order

create :: DOrder.PaymentOrder -> SqlDB ()
create = Esq.create

updateStatus :: DOrder.PaymentOrder -> SqlDB ()
updateStatus order = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PaymentOrderStatus =. val order.status,
        PaymentOrderUpdatedAt =. val now
      ]
    where_ $ tbl ^. PaymentOrderId ==. val order.id.getId
