module Lib.JourneyLeg.Types.Metro where

import qualified Domain.Types.FRFSSearch as FRFSSearch
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id

data MetroLegRequestSearchData = MetroLegRequestSearchData
  { quantity :: Int,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    city :: Context.City,
    journeyLeg :: DJourneyLeg.JourneyLeg
  }

data MetroLegRequestUpdateData = MetroLegRequestUpdateData

data MetroLegRequestConfirmData = MetroLegRequestConfirmData

data MetroLegRequestCancelData = MetroLegRequestCancelData

data MetroLegRequestGetStateData = MetroLegRequestGetStateData

newtype MetroLegRequestGetInfoData = MetroLegRequestGetInfoData
  { searchId :: Id FRFSSearch.FRFSSearch
  }

data MetroLegRequest
  = MetroLegRequestSearch MetroLegRequestSearchData
  | MetroLegRequestConfirm MetroLegRequestConfirmData
  | MetroLegRequestUpdate MetroLegRequestUpdateData
  | MetroLegRequestCancel MetroLegRequestCancelData
  | MetroLegRequestGetFare MetroLegRequestGetFareData
  | MetroLegRequestGetState MetroLegRequestGetStateData
  | MetroLegRequestGetInfo MetroLegRequestGetInfoData

data MetroLegRequestGetFareData = MetroLegRequestGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2
  }
