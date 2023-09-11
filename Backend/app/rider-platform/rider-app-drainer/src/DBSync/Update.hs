{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module DBSync.Update where

import Config.Env
import Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Extra (mapLeft)
import Data.Maybe (fromJust)
import Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Beam as B hiding (runUpdate)
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.KVConnector.DBSync
import EulerHS.KVConnector.Types
import EulerHS.KVConnector.Utils as EKU
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import EulerHS.Types as ET
import Kafka.Producer as KafkaProd
import Kafka.Producer as Producer
import qualified Kernel.Beam.Types as KBT
import Sequelize (Model, Set, Where)
import System.Timeout (timeout)
import Text.Casing
import Types.DBSync
import Types.Event as Event
import Utils.Utils

updateDB ::
  forall beM be table m.
  ( HasCallStack,
    ET.BeamRuntime be beM,
    ET.BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    EL.MonadFlow m,
    ToJSON (table Identity),
    FromJSON (table Identity)
  ) =>
  ET.DBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  ByteString ->
  m (Either MeshError [A.Value])
updateDB dbConf _ setClause whereClause bts = do
  either (pure . Left) ((Right <$>) . mapM updateModel') . mapLeft MDBError
    =<< runExceptT
      ( do
          updateObj <- ExceptT $ CDB.findAll dbConf Nothing whereClause
          ExceptT $ CDB.updateOneWoReturning dbConf Nothing setClause whereClause
          pure updateObj
      )
  where
    updateModel' model = do
      let val = (EKU.updateModel @be @table) model (EKU.jsonKeyValueUpdates setClause)
      case val of
        Right obj -> pure obj
        Left err -> do
          EL.logError (("Model Update failed: " :: Text) <> T.pack (show err)) (show [("command" :: String, bts)] :: Text)
          pure A.Null

runUpdateCommands :: (UpdateDBCommand, ByteString) -> Text -> Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runUpdateCommands (cmd, val) streamKey = do
  let dbConf = fromJust <$> EL.getOption KBT.PsqlDbCfg
  case cmd of
    UpdateDBCommand id _ _ _ _ (AppInstallsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("AppInstalls" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BlackListOrgOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("BlackListOrg" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BookingOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("Booking" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BookingLocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("BookingLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BookingCancellationReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("BookingCancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (CallbackRequestOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("CallbackRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (CallStatusOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("CallStatus" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (CancellationReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("CancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DriverOfferOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("DriverOffer" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (EstimateOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("Estimate" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (EstimateBreakupOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("EstimateBreakup" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (ExophoneOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("Exophone" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FareBreakupOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("FareBreakup" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (GeometryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("Geometry" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (IssueOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("Issue" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (DirectionsCacheOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("DirectionsCache" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (PlaceNameCacheOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("PlaceNameCache" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("Merchant" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantMessageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("MerchantMessage" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantPaymentMethodOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("MerchantPaymentMethod" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantServiceConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("MerchantServiceConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantServiceUsageConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("MerchantServiceUsageConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (MerchantConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("MerchantConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (OnSearchEventOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("OnSearchEvent" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (PaymentOrderOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("PaymentOrder" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (PaymentTransactionOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("PaymentTransaction" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (PersonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("Person" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (PersonDefaultEmergencyNumberOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("PersonDefaultEmergencyNumber" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (PersonFlowStatusOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("PersonFlowStatus" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (QuoteOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("Quote" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RegistrationTokenOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("RegistrationToken" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RentalSlabOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("RentalSlab" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (RideOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("Ride" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SavedReqLocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("SavedReqLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchRequestOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("SearchRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SearchReqLocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("SearchReqLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SosOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("Sos" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (SpecialZoneQuoteOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("SpecialZoneQuote" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (TripTermsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("TripTerms" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (WebengageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("Webengage" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (FeedbackFormOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("FeedbackForm" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (HotSpotConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("HotSpotConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BecknRequestOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val streamKey setClauses whereClause ("BecknRequest" :: Text) =<< dbConf
  where
    runUpdate id value _ setClause whereClause model dbConf = do
      maxRetries <- EL.runIO getMaxRetries
      runUpdateWithRetries id value setClause whereClause model dbConf 0 maxRetries

    runUpdateInKafka id value streamKey' setClause whereClause model dbConf = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runUpdate id value streamKey' setClause whereClause model dbConf
        else do
          let setAndWhere = getDbUpdateDataJson model (jsonKeyValueUpdates setClause) whereClause
          Env {..} <- ask
          res <- EL.runIO $ streamDriverDrainerUpdates _kafkaConnection setAndWhere streamKey'
          either (\err -> pure $ Left (UnexpectedError err, id)) (\_ -> pure $ Right id) res

    runUpdateInKafkaAndDb id value streamKey' setClause whereClause model dbConf = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runUpdate id value streamKey' setClause whereClause model dbConf
        else do
          res <- runUpdateInKafka id value streamKey' setClause whereClause model dbConf
          case res of
            Left (err, id') -> pure $ Left (err, id')
            Right id' -> pure $ Right id'

    runUpdateWithRetries id value setClause whereClause model dbConf retryIndex maxRetries = do
      res <- updateDB dbConf Nothing setClause whereClause value
      case (res, retryIndex) of
        (Left _, y) | y < maxRetries -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" model
          EL.runIO $ delay =<< getRetryDelay
          runUpdateWithRetries id value setClause whereClause model dbConf (retryIndex + 1) maxRetries
        (Left x, _) -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" model
          EL.logError (("Update failed: " :: Text) <> T.pack (show x)) (show [("command" :: String, value)] :: Text)
          pure $ Left (x, id)
        (Right _, _) -> do
          pure $ Right id

streamDriverDrainerUpdates :: ToJSON a => Producer.KafkaProducer -> a -> Text -> IO (Either Text ())
streamDriverDrainerUpdates producer dbObject dbStreamKey = do
  let topicName = "rider-drainer"
  void $ KafkaProd.produceMessage producer (message topicName dbObject)
  flushResult <- timeout (5 * 60 * 1000000) $ prodPush producer
  case flushResult of
    Just _ -> do
      pure $ Right ()
    Nothing -> pure $ Left "KafkaProd.flushProducer timed out after 5 minutes"
  where
    prodPush producer' = KafkaProd.flushProducer producer' >> pure True

    message topicName event =
      ProducerRecord
        { prTopic = TopicName topicName,
          prPartition = UnassignedPartition,
          prKey = Just $ TE.encodeUtf8 dbStreamKey,
          prValue = Just . LBS.toStrict $ encode event
        }

getDbUpdateDataJson :: forall be table. (Model be table, MeshMeta be table) => Text -> [(Text, A.Value)] -> Where be table -> A.Value
getDbUpdateDataJson model upd whereClause =
  A.object
    [ "contents"
        .= A.object
          [ "set" .= A.object [k .= v | (k, v) <- upd],
            "where" .= modelEncodeWhere whereClause
          ],
      "tag" .= T.pack (pascal (T.unpack model)),
      "type" .= ("UPDATE" :: Text)
    ]
