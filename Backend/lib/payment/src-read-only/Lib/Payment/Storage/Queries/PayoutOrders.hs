{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutOrders where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutOrders
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutOrders as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders] -> m ())
createMany = traverse_ create

findById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders -> m (Maybe Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByOrderId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders))
findByOrderId orderId = do findOneWithKV [Se.Is Beam.orderId $ Se.Eq orderId]

updatePayoutOrderStatus :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ())
updatePayoutOrderStatus orderId = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.updatedAt _now] [Se.Is Beam.orderId $ Se.Eq orderId]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders -> Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders))
findByPrimaryKey (Kernel.Types.Id.Id id) orderId = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id, Se.Is Beam.orderId $ Se.Eq orderId]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.accountDetailsType accountDetailsType,
      Se.Set Beam.amount amount,
      Se.Set Beam.city city,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.customerEmail customerEmail,
      Se.Set Beam.customerId customerId,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.mobileNo mobileNo,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vpa vpa
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.orderId $ Se.Eq orderId]]

instance FromTType' Beam.PayoutOrders Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders where
  fromTType' (Beam.PayoutOrdersT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders
          { accountDetailsType = accountDetailsType,
            amount = amount,
            city = city,
            createdAt = createdAt,
            customerEmail = customerEmail,
            customerId = customerId,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            mobileNo = mobileNo,
            orderId = orderId,
            status = status,
            updatedAt = updatedAt,
            vpa = vpa
          }

instance ToTType' Beam.PayoutOrders Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders where
  toTType' (Lib.Payment.Domain.Types.PayoutOrders.PayoutOrders {..}) = do
    Beam.PayoutOrdersT
      { Beam.accountDetailsType = accountDetailsType,
        Beam.amount = amount,
        Beam.city = city,
        Beam.createdAt = createdAt,
        Beam.customerEmail = customerEmail,
        Beam.customerId = customerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.mobileNo = mobileNo,
        Beam.orderId = orderId,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.vpa = vpa
      }
