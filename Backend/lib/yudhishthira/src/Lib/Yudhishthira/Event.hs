module Lib.Yudhishthira.Event where

import qualified Data.Aeson as A
import Data.Scientific
import JsonLogic
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.NammaTag as SQNT
-- import Lib.Yudhishthira.Tools.Utils
import Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTag as DNT

yudhishthiraDecide ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasYudhishthiraTablesSchema
    -- HasCacConfig r
  ) =>
  YudhishthiraDecideReq ->
  m YudhishthiraDecideResp
yudhishthiraDecide req = do
  nammaTags <-
    case req.source of
      Application event -> SQNT.findAllByApplicationEvent event
      KaalChakra chakra -> SQNT.findAllByChakra chakra
  logDebug $ "NammaTags for source <> " <> show req.source <> ": " <> show nammaTags
  logDebug $ "SourceData: " <> show req.sourceData
  tags <- convertToTagResponses nammaTags
  return $ YudhishthiraDecideResp {..}
  where
    convertToTagResponses ::
      (MonadFlow m) =>
      [DNT.NammaTag] ->
      m [NammaTagResponse]
    convertToTagResponses tags = do
      mbTagResponses <- mapM convertToTagResponse tags
      return $ catMaybes mbTagResponses

    convertToTagResponse :: (MonadFlow m) => DNT.NammaTag -> m (Maybe NammaTagResponse)
    convertToTagResponse tag = do
      let tagValidity = case tag.info of
            DNT.KaalChakra (DNT.KaalChakraTagInfo _ validity) -> validity
            _ -> Nothing
      respValue <-
        case tag.rule of
          LLM context -> throwError $ InternalError $ "LLM not supported yet: " <> show context
          RuleEngine rule -> jsonLogic rule req.sourceData
      logDebug $ "Tag: " <> show tag <> " jsonResp: " <> show respValue
      mbTagValue <- case respValue of
        A.String text -> return $ Just (TextValue text)
        A.Number number -> do
          let doubleValue = toRealFloat number -- :: Maybe Int = toBoundedInteger number
          return $ Just (NumberValue doubleValue)
        value -> do
          logError $ "Invalid value for tag: " <> show value
          return Nothing
      logDebug $ "Tag: " <> show tag <> " Value: " <> show mbTagValue
      return $
        mbTagValue
          <&> \tagValue ->
            NammaTagResponse
              { tagName = tag.name,
                tagValue,
                tagCategory = tag.category,
                tagValidity
              }

computeNammaTags ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasYudhishthiraTablesSchema,
    ToJSON a
  ) =>
  ApplicationEvent ->
  a ->
  m [Text]
computeNammaTags event sourceData_ = do
  let sourceData = A.toJSON sourceData_
  let req = YudhishthiraDecideReq {source = Application event, sourceData}
  resp <- yudhishthiraDecide req
  resp.tags `forM` \tag -> do
    tagValue <- case tag.tagValue of
      TextValue text -> return text
      NumberValue number -> return $ show number
    return $ tag.tagName <> "#" <> tagValue
