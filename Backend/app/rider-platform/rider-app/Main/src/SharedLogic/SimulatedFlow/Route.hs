module SharedLogic.SimulatedFlow.Route where

import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Types.Merchant (Merchant)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Tools.Maps as Maps
import Tools.Metrics (CoreMetrics)

simulateRoute :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Bool -> Id Merchant -> Maps.GetRoutesReq -> m Maps.GetRoutesResp -> m Maps.GetRoutesResp
simulateRoute isSimulated merchantId req nonSimulatedAction = if isSimulated then Maps.getSimulatedRoutes merchantId req else nonSimulatedAction
