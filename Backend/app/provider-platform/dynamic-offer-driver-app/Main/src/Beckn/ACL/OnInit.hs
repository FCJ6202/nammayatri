{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnInit where

import qualified Beckn.ACL.Common as Common
import Beckn.Types.Core.Taxi.OnInit as OnInit
import Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.FareParameters as DFParams
import Kernel.Prelude
import SharedLogic.FareCalculator

mkOnInitMessage :: DInit.InitRes -> OnInit.OnInitMessage
mkOnInitMessage res = do
  let rb = res.booking
      fareDecimalValue = fromIntegral rb.estimatedFare
      currency = "INR"
      breakup_ =
        mkBreakupList (OnInit.BreakupItemPrice currency . fromIntegral) OnInit.BreakupItem rb.fareParams
          & filter (Common.filterRequiredBreakups $ DFParams.getFareParametersType rb.fareParams) -- TODO: Remove after roll out
  OnInit.OnInitMessage
    { order =
        OnInit.Order
          { id = res.booking.id.getId,
            state = OnInit.NEW,
            quote =
              OnInit.Quote
                { price =
                    OnInit.QuotePrice
                      { currency,
                        value = fareDecimalValue,
                        offered_value = fareDecimalValue
                      },
                  breakup = breakup_
                },
            payment =
              OnInit.Payment
                { collected_by = maybe OnInit.BPP (Common.castDPaymentCollector . (.collectedBy)) res.paymentMethodInfo,
                  params =
                    OnInit.PaymentParams
                      { currency = currency,
                        amount = fareDecimalValue
                      },
                  _type = maybe OnInit.ON_FULFILLMENT (Common.castDPaymentType . (.paymentType)) res.paymentMethodInfo,
                  instrument = Common.castDPaymentInstrument . (.paymentInstrument) <$> res.paymentMethodInfo,
                  time = OnInit.TimeDuration "FIXME",
                  uri = res.paymentUrl
                }
          }
    }
