{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.OnSearchEvent where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data OnSearchEvent = OnSearchEvent
  { id :: Kernel.Types.Id.Id Domain.Types.OnSearchEvent.OnSearchEvent,
    bppId :: Kernel.Prelude.Text,
    messageId :: Kernel.Prelude.Text,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
