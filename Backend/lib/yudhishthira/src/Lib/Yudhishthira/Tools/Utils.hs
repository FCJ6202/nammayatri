module Lib.Yudhishthira.Tools.Utils where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as DBL
import qualified Data.String.Conversions as CS
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Lazy as DTE
import qualified Data.Text.Lazy.Encoding as DTLE
import JsonLogic
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogic as DAL
import Lib.Yudhishthira.Storage.Queries.ChakraQueries as SQCQ
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.AppDynamicLogic

mandatoryChakraFields :: [Text]
mandatoryChakraFields = [userIdField]

userIdField :: Text
userIdField = "userId"

getChakraQueryFields :: BeamFlow m r => LYT.Chakra -> m [Text]
getChakraQueryFields chakra = do
  queries <- SQCQ.findAllByChakra chakra
  return $ filter (\field -> field `notElem` mandatoryChakraFields) $ concatMap (.queryResults) queries

decodeTextToValue :: Text -> Either String Value
decodeTextToValue text =
  let byteString = DTLE.encodeUtf8 $ DTE.fromStrict text
   in A.eitherDecode byteString

getAppDynamicLogic ::
  BeamFlow m r =>
  Id LYT.MerchantOperatingCity ->
  LYT.LogicDomain ->
  UTCTime ->
  m [AppDynamicLogic]
getAppDynamicLogic merchantOpCityId domain localTime = do
  mbConfigs <- DAL.findByMerchantOpCityAndDomain merchantOpCityId domain
  configs <- if null mbConfigs then DAL.findByMerchantOpCityAndDomain (Id "default") domain else return mbConfigs
  let boundedConfigs = findBoundedDomain (filter (\cfg -> cfg.timeBounds /= Unbounded) configs) localTime
  return $ if null boundedConfigs then filter (\cfg -> cfg.timeBounds == Unbounded) configs else boundedConfigs

runJsonLogic :: (MonadFlow m) => Value -> Text -> m A.Value
runJsonLogic data' ruleText = do
  let eitherRule = decodeTextToValue ruleText
  rule <-
    case eitherRule of
      Right rule -> return rule
      Left err -> throwError $ InternalError ("Unable to decode rule:" <> show err)
  jsonLogic rule data'

runLogics :: (MonadFlow m, ToJSON a) => [A.Value] -> a -> m LYT.RunLogicResp
runLogics logics data_ = do
  let logicData = A.toJSON data_
  logDebug $ "logics- " <> show logics
  logDebug $ "logicData- " <> CS.cs (A.encode logicData)
  let startingPoint = LYT.RunLogicResp logicData []
  foldlM
    ( \acc logic -> do
        let result = jsonLogicEither logic acc.result
        res <-
          case result of
            Left err -> do
              logError $ "Got error: " <> show err <> " while running logic: " <> CS.cs (A.encode logics)
              pure $ LYT.RunLogicResp acc.result (acc.errors <> [show err])
            Right res -> pure $ LYT.RunLogicResp res acc.errors
        logDebug $ "logic- " <> (CS.cs . A.encode $ logic)
        logDebug $ "json logic result - " <> (CS.cs . A.encode $ res)
        return res
    )
    startingPoint
    logics

decodeText :: Text -> Maybe A.Value
decodeText txt = A.decode (DBL.fromStrict . DTE.encodeUtf8 $ txt)

-- Function to convert Text to Maybe Value
textToMaybeValue :: Text -> Maybe A.Value
textToMaybeValue txt =
  case decodeText txt of
    Just value -> Just value
    Nothing -> decodeText (T.concat ["\"", txt, "\""])
