module Lib.Mesh where

import EulerHS.KVConnector.Types (MeshConfig (..))
import Kernel.Prelude hiding (Generic)

meshConfig :: MeshConfig
meshConfig =
  MeshConfig
    { meshEnabled = False,
      memcacheEnabled = False,
      meshDBName = "ECRDB",
      ecRedisDBStream = "db-sync-stream",
      kvRedis = "KVRedis",
      redisTtl = 43200,
      kvHardKilled = True,
      cerealEnabled = False
    }
