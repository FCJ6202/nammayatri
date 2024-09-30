{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TimeBoundConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data TimeBoundConfigT f = TimeBoundConfigT
  { merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    name :: (B.C f Kernel.Prelude.Text),
    timeBoundDomain :: (B.C f Lib.Yudhishthira.Types.LogicDomain),
    timeBounds :: (B.C f Kernel.Types.TimeBound.TimeBound),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table TimeBoundConfigT where
  data PrimaryKey TimeBoundConfigT f = TimeBoundConfigId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Lib.Yudhishthira.Types.LogicDomain) deriving (Generic, B.Beamable)
  primaryKey = TimeBoundConfigId <$> merchantOperatingCityId <*> name <*> timeBoundDomain

type TimeBoundConfig = TimeBoundConfigT Identity

$(enableKVPG (''TimeBoundConfigT) [('merchantOperatingCityId), ('name), ('timeBoundDomain)] [])

$(mkTableInstances (''TimeBoundConfigT) "time_bound_config")
