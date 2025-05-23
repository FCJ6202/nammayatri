{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Fleet.Registration where

import qualified API.Types.UI.DriverOnboardingV2 as DO
import Data.OpenApi (ToSchema)
import Data.Time hiding (getCurrentTime)
import Domain.Action.Dashboard.Fleet.Referral
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.Referral as DOR
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DVRC
import qualified Domain.Action.UI.DriverOnboardingV2 as Registration
import qualified Domain.Action.UI.DriverReferral as DR
import qualified Domain.Action.UI.Registration as Registration
import qualified Domain.Types.DocumentVerificationConfig as DVC
import Domain.Types.FleetOperatorAssociation
import Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (getDbHash)
import Kernel.Sms.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified SharedLogic.DriverOnboarding as DomainRC
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.SMS as Sms hiding (Success)

---------------------------------------------------------------------
data FleetOwnerLoginReq = FleetOwnerLoginReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text,
    merchantId :: Text,
    otp :: Maybe Text,
    city :: Context.City
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data UpdateFleetOwnerReq = UpdateFleetOwnerReq
  { firstName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data FleetOwnerRegisterReq = FleetOwnerRegisterReq
  { firstName :: Text,
    lastName :: Text,
    personId :: Id DP.Person,
    merchantId :: Text,
    email :: Maybe Text,
    city :: City.City,
    fleetType :: Maybe FOI.FleetType,
    panNumber :: Maybe Text,
    gstNumber :: Maybe Text,
    businessLicenseNumber :: Maybe Text,
    panImageId1 :: Maybe Text,
    panImageId2 :: Maybe Text,
    gstCertificateImage :: Maybe Text,
    businessLicenseImage :: Maybe Text,
    operatorReferralCode :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

newtype FleetOwnerRegisterRes = FleetOwnerRegisterRes
  { personId :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

newtype FleetOwnerVerifyRes = FleetOwnerVerifyRes
  { authToken :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

fleetOwnerRegister :: FleetOwnerRegisterReq -> Flow APISuccess
fleetOwnerRegister req = do
  person <- QP.findById req.personId >>= fromMaybeM (PersonDoesNotExist req.personId.getId)
  fleetOwnerInfo <- QFOI.findByPrimaryKey req.personId >>= fromMaybeM (PersonDoesNotExist req.personId.getId)
  void $ QP.updateByPrimaryKey person{firstName = req.firstName, lastName = Just req.lastName}
  void $ updateFleetOwnerInfo fleetOwnerInfo req
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  mbReferredOperatorId <- getOperatorIdFromReferralCode req.operatorReferralCode
  whenJust mbReferredOperatorId $ \referredOperatorId -> do
    fleetAssocs <- QFOA.findAllFleetAssociations req.personId.getId
    when (null fleetAssocs) $ do
      fleetOperatorAssocData <- makeFleetOperatorAssociation person.merchantId person.merchantOperatingCityId (req.personId.getId) referredOperatorId (DomainRC.convertTextToUTC (Just "2099-12-12"))
      QFOA.create fleetOperatorAssocData
      DOR.incrementOnboardedCount DOR.FleetReferral (Id referredOperatorId) transporterConfig
  when (transporterConfig.generateReferralCodeForFleet == Just True) $ do
    fleetReferral <- QDR.findById person.id
    when (isNothing fleetReferral) $ void $ DR.generateReferralCode (Just DP.FLEET_OWNER) (req.personId, person.merchantId, person.merchantOperatingCityId)
  whenJust req.panNumber $ \panNumber -> do
    createPanInfo req.personId person.merchantId person.merchantOperatingCityId req.panImageId1 req.panImageId2 panNumber

  whenJust req.gstCertificateImage $ \gstImage -> do
    let req' = Image.ImageValidateRequest {imageType = DVC.GSTCertificate, image = gstImage, rcNumber = Nothing, validationStatus = Nothing, workflowTransactionId = Nothing, vehicleCategory = Nothing, sdkFailureReason = Nothing}
    image <- Image.validateImage True (req.personId, person.merchantId, person.merchantOperatingCityId) req'
    void $ DVRC.verifyGstin True (Nothing) (req.personId, person.merchantId, person.merchantOperatingCityId) (DVRC.DriverGstinReq {gstin = fromMaybe "" req.gstNumber, imageId = gstImage, driverId = req.personId.getId})
    QFOI.updateGstImage req.gstNumber (Just image.imageId.getId) req.personId
  fork "Uploading Business License Image" $ do
    whenJust req.businessLicenseImage $ \businessLicenseImage -> do
      let req' = Image.ImageValidateRequest {imageType = DVC.BusinessLicense, image = businessLicenseImage, rcNumber = Nothing, validationStatus = Nothing, workflowTransactionId = Nothing, vehicleCategory = Nothing, sdkFailureReason = Nothing}
      image <- Image.validateImage True (req.personId, person.merchantId, person.merchantOperatingCityId) req'
      QFOI.updateBusinessLicenseImage (Just image.imageId.getId) req.personId
  return Success

getOperatorIdFromReferralCode :: Maybe Text -> Flow (Maybe Text)
getOperatorIdFromReferralCode Nothing = return Nothing
getOperatorIdFromReferralCode (Just refCode) = do
  let referralReq = FleetReferralReq {value = refCode}
  result <- isValidReferralForRole referralReq DP.OPERATOR
  case result of
    SuccessCode val -> return $ Just val

makeFleetOperatorAssociation :: (MonadFlow m) => Id DMerchant.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Maybe UTCTime -> m FleetOperatorAssociation
makeFleetOperatorAssociation merchantId merchantOpCityId fleetOwnerId operatorId end = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    FleetOperatorAssociation
      { id = id,
        operatorId = operatorId,
        isActive = True,
        fleetOwnerId = fleetOwnerId,
        associatedOn = Just now,
        associatedTill = end,
        createdAt = now,
        updatedAt = now,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOpCityId
      }

createFleetOwnerDetails :: Registration.AuthReq -> Id DMerchant.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Text -> Maybe Bool -> Flow DP.Person
createFleetOwnerDetails authReq merchantId merchantOpCityId isDashboard deploymentVersion enabled = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  person <- Registration.makePerson authReq transporterConfig Nothing Nothing Nothing Nothing (Just deploymentVersion) merchantId merchantOpCityId isDashboard (Just DP.FLEET_OWNER)
  void $ QP.create person
  merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  QDriverStats.createInitialDriverStats merchantOperatingCity.currency merchantOperatingCity.distanceUnit person.id
  fork "creating fleet owner info" $ createFleetOwnerInfo person.id merchantId enabled
  pure person

createPanInfo :: Id DP.Person -> Id DMerchant.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> Maybe Text -> Text -> Flow ()
createPanInfo personId merchantId merchantOperatingCityId (Just img1) _ panNo = do
  let req' = Image.ImageValidateRequest {imageType = DVC.PanCard, image = img1, rcNumber = Nothing, validationStatus = Nothing, workflowTransactionId = Nothing, vehicleCategory = Nothing, sdkFailureReason = Nothing}
  image <- Image.validateImage True (personId, merchantId, merchantOperatingCityId) req'
  let panReq = DO.DriverPanReq {panNumber = panNo, imageId1 = image.imageId, imageId2 = Nothing, consent = True, nameOnCard = Nothing, dateOfBirth = Nothing, consentTimestamp = Nothing, validationStatus = Nothing, verifiedBy = Nothing, transactionId = Nothing, nameOnGovtDB = Nothing, docType = Nothing}
  void $ Registration.postDriverRegisterPancard (Just personId, merchantId, merchantOperatingCityId) panReq
createPanInfo _ _ _ _ _ _ = pure () --------- currently we can have it like this as Pan info is optional

createFleetOwnerInfo :: Id DP.Person -> Id DMerchant.Merchant -> Maybe Bool -> Flow ()
createFleetOwnerInfo personId merchantId enabled = do
  now <- getCurrentTime
  let fleetOwnerInfo =
        FOI.FleetOwnerInformation
          { fleetOwnerPersonId = personId,
            merchantId = merchantId,
            fleetType = NORMAL_FLEET, -- overwrite in register
            enabled = fromMaybe True enabled,
            blocked = False,
            verified = False,
            gstNumber = Nothing,
            gstImageId = Nothing,
            businessLicenseImageId = Nothing,
            businessLicenseNumber = Nothing,
            referredByOperatorId = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QFOI.create fleetOwnerInfo

fleetOwnerLogin ::
  Maybe Bool ->
  FleetOwnerLoginReq ->
  Flow FleetOwnerRegisterRes
fleetOwnerLogin enabled req = do
  runRequestValidation validateInitiateLoginReq req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
      merchantId = ShortId req.merchantId
  merchant <- QMerchant.findByShortId merchantId >>= fromMaybeM (MerchantNotFound merchantId.getShortId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just req.city)
  mobileNumberHash <- getDbHash mobileNumber
  mbPerson <- QP.findByMobileNumberAndMerchantAndRoles req.mobileCountryCode mobileNumberHash merchant.id [DP.FLEET_OWNER, DP.OPERATOR]
  personId <- case mbPerson of
    Just person -> pure person.id
    Nothing -> do
      -- Operator won't reach here as it has separate sign up flow --
      let personAuth = buildFleetOwnerAuthReq merchant.id req
      deploymentVersion <- asks (.version)
      person <- createFleetOwnerDetails personAuth merchant.id merchantOpCityId True deploymentVersion.getDeploymentVersion enabled
      pure person.id
  let useFakeOtpM = useFakeSms smsCfg
  otp <- maybe generateOTPCode (return . show) useFakeOtpM
  whenNothing_ useFakeOtpM $ do
    let otpHash = smsCfg.credConfig.otpHash
    let otpCode = otp
        phoneNumber = countryCode <> mobileNumber
    withLogTag ("mobileNumber" <> req.mobileNumber) $
      do
        (mbSender, message) <-
          MessageBuilder.buildSendOTPMessage merchantOpCityId $
            MessageBuilder.BuildSendOTPMessageReq
              { otp = otpCode,
                hash = otpHash
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
        >>= Sms.checkSmsResult
  let key = makeMobileNumberOtpKey mobileNumber
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  void $ Redis.setExp key otp expTime
  pure $ FleetOwnerRegisterRes {personId = personId.getId}

buildFleetOwnerAuthReq ::
  Id DMerchant.Merchant ->
  FleetOwnerLoginReq ->
  Registration.AuthReq
buildFleetOwnerAuthReq merchantId' FleetOwnerLoginReq {..} =
  Registration.AuthReq
    { name = Just "Fleet Owner", -- to be updated in register
      mobileNumber = Just mobileNumber,
      mobileCountryCode = Just mobileCountryCode,
      merchantId = merchantId'.getId,
      merchantOperatingCity = Just city,
      identifierType = Just DP.MOBILENUMBER,
      email = Nothing,
      registrationLat = Nothing,
      registrationLon = Nothing
    }

updateFleetOwnerInfo ::
  FOI.FleetOwnerInformation ->
  FleetOwnerRegisterReq ->
  Flow ()
updateFleetOwnerInfo fleetOwnerInfo FleetOwnerRegisterReq {..} = do
  let updFleetOwnerInfo =
        fleetOwnerInfo
          { FOI.fleetType = fromMaybe fleetOwnerInfo.fleetType fleetType,
            FOI.gstNumber = gstNumber,
            FOI.gstImageId = gstCertificateImage,
            FOI.businessLicenseImageId = businessLicenseImage
          }
  void $ QFOI.updateByPrimaryKey updFleetOwnerInfo

fleetOwnerVerify ::
  FleetOwnerLoginReq ->
  Flow APISuccess
fleetOwnerVerify req = do
  case req.otp of
    Just otp -> do
      mobileNumberOtpKey <- Redis.safeGet $ makeMobileNumberOtpKey req.mobileNumber
      case mobileNumberOtpKey of
        Just otpHash -> do
          unless (otpHash == otp) $ throwError InvalidAuthData
          let merchantId = ShortId req.merchantId
          merchant <-
            QMerchant.findByShortId merchantId
              >>= fromMaybeM (MerchantNotFound merchantId.getShortId)
          mobileNumberHash <- getDbHash req.mobileNumber
          person <- QP.findByMobileNumberAndMerchantAndRoles req.mobileCountryCode mobileNumberHash merchant.id [DP.FLEET_OWNER, DP.OPERATOR] >>= fromMaybeM (PersonNotFound req.mobileNumber)
          -- currently we don't create fleetOwnerInfo for operator
          when (person.role == DP.FLEET_OWNER) $
            void $ QFOI.updateFleetOwnerVerifiedStatus True person.id
          pure Success
        Nothing -> throwError InvalidAuthData
    _ -> throwError InvalidAuthData

makeMobileNumberOtpKey :: Text -> Text
makeMobileNumberOtpKey mobileNumber = "MobileNumberOtp:mobileNumber-" <> mobileNumber

validateInitiateLoginReq :: Validate FleetOwnerLoginReq
validateInitiateLoginReq FleetOwnerLoginReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]
