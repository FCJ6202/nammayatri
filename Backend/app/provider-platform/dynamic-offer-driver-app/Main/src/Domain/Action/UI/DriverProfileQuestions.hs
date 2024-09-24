{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.DriverProfileQuestions where

import qualified API.Types.UI.DriverProfileQuestions
import qualified AWS.S3 as S3
import Data.Maybe (fromJust)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time.Calendar (diffDays)
import Data.Time.Clock (utctDay)
import qualified Domain.Types.DriverProfileQuestions as DTDPQ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import Kernel.Beam.Functions as B
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common as KUC
import Servant
import Storage.Beam.IssueManagement ()
import qualified Storage.Queries.DriverProfileQuestions as DPQ
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Person as QP
import Tools.Auth
import Tools.Error

data ImageType = JPG | PNG | UNKNOWN deriving (Generic, Show, Eq)

postDriverProfileQues ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverProfileQuestions.DriverProfileQuesReq ->
    Flow APISuccess
  )
postDriverProfileQues (mbPersonId, _, merchantOpCityId) req@API.Types.UI.DriverProfileQuestions.DriverProfileQuesReq {..} =
  do
    driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
    person <- QP.findById driverId >>= fromMaybeM (PersonNotFound ("No person found with id" <> show driverId))
    driverStats <- QDS.findByPrimaryKey driverId >>= fromMaybeM (PersonNotFound ("No person found with id" <> show driverId))
    now <- getCurrentTime
    DPQ.upsert
      ( DTDPQ.DriverProfileQuestions
          { updatedAt = now,
            createdAt = now,
            driverId = driverId,
            hometown = hometown,
            merchantOperatingCityId = merchantOpCityId,
            pledges = pledges,
            aspirations = toMaybe aspirations,
            drivingSince = drivingSince,
            imageIds = toMaybe imageIds,
            vehicleTags = toMaybe vehicleTags,
            aboutMe = generateAboutMe person driverStats now req
          }
      )
      >> pure Success
  where
    toMaybe xs = guard (not (null xs)) >> Just xs

    -- Generate with LLM or create a template text here
    generateAboutMe person driverStats now req' = Just (hometownDetails req'.hometown <> "I have been with Nammayatri for " <> (withNY now person.createdAt) <> " months. " <> writeDriverStats driverStats <> genAspirations req'.aspirations)

    hometownDetails mHometown = case mHometown of
      Just hometown' -> "Hailing from " <> hometown' <> ", "
      Nothing -> ""

    withNY now createdAt = T.pack $ show $ diffDays (utctDay now) (utctDay createdAt) `div` 30

    writeDriverStats driverStats = ratingStat driverStats <> cancellationStat driverStats

    nonZero Nothing = 1
    nonZero (Just a)
      | a <= 0 = 1
      | otherwise = a

    ratingStat driverStats =
      if driverStats.rating > Just 4.75 && isJust driverStats.rating
        then "I rank among the top 10 percentile in terms of rating "
        else ""

    cancellationStat driverStats =
      let cancRate = div ((fromMaybe 0 driverStats.ridesCancelled) * 100 :: Int) (nonZero driverStats.totalRidesAssigned :: Int)
       in if cancRate < 7
            then "I " <> if (ratingStat driverStats :: Text) == "" then "" else "also " <> "have a very low cancellation rate that ranks among top 10 percentile. "
            else ""

    genAspirations aspirations' = if null aspirations' then "" else "With the earnings from my trips, I aspire to " <> T.toLower (T.intercalate ", " aspirations')

getDriverProfileQues ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Maybe Bool ->
    Flow API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
  )
getDriverProfileQues (mbPersonId, _merchantId, _merchantOpCityId) isImages =
  mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
    >>= DPQ.findByPersonId
    >>= \case
      Just res ->
        getImages (maybe [] (Id <$>) res.imageIds)
          >>= \images ->
            pure $
              API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
                { aspirations = fromMaybe [] res.aspirations,
                  hometown = res.hometown,
                  pledges = res.pledges,
                  drivingSince = res.drivingSince,
                  vehicleTags = fromMaybe [] res.vehicleTags,
                  otherImages = if isImages == Just True then images else [], -- fromMaybe [] res.images
                  profileImage = Nothing,
                  otherImageIds = fromMaybe [] res.imageIds
                }
      Nothing ->
        pure $
          API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
            { aspirations = [],
              hometown = Nothing,
              pledges = [],
              drivingSince = Nothing,
              vehicleTags = [],
              otherImages = [],
              profileImage = Nothing,
              otherImageIds = []
            }
  where
    getImages imageIds = do
      mapM (QMF.findById) imageIds <&> catMaybes <&> ((.url) <$>)
        >>= mapM (S3.get . T.unpack . extractFilePath)

    extractFilePath url = case T.splitOn "filePath=" url of
      [_before, after] -> after
      _ -> T.empty
