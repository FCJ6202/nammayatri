{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Vehicle where

import qualified Database.Beam as B
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleT f = VehicleT
  { airConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    capacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    category :: B.C f (Kernel.Prelude.Maybe Domain.Types.Vehicle.Category),
    color :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    energyType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    luggageCapacity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    make :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    model :: B.C f Kernel.Prelude.Text,
    registrationCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Vehicle.RegistrationCategory),
    registrationNo :: B.C f Kernel.Prelude.Text,
    size :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    variant :: B.C f Domain.Types.Vehicle.Variant,
    vehicleClass :: B.C f Kernel.Prelude.Text,
    vehicleName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleRating :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleT where
  data PrimaryKey VehicleT f = VehicleId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleId . driverId

type Vehicle = VehicleT Identity

$(enableKVPG ''VehicleT ['driverId] [['registrationNo]])

$(mkTableInstances ''VehicleT "vehicle")
