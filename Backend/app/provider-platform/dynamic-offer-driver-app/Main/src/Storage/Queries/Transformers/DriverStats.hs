{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.DriverStats where

import GHC.Float (int2Double)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)

getTotalDistance :: (Kernel.Types.Common.Meters -> Kernel.Prelude.Double)
getTotalDistance totalDistance = (\(Kernel.Types.Common.Meters m) -> int2Double m) totalDistance
