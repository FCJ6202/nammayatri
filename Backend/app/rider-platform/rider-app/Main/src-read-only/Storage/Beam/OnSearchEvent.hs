{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.OnSearchEvent where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data OnSearchEventT f = OnSearchEventT
  { id :: B.C f Kernel.Prelude.Text,
    bppId :: B.C f Kernel.Prelude.Text,
    messageId :: B.C f Kernel.Prelude.Text,
    errorCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    errorType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    errorMessage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table OnSearchEventT where
  data PrimaryKey OnSearchEventT f = OnSearchEventId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OnSearchEventId . id

type OnSearchEvent = OnSearchEventT Identity

$(enableKVPG ''OnSearchEventT ['id] [])

$(mkTableInstances ''OnSearchEventT "on_search_event")
