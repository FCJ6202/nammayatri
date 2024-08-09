module Tools.Utils where

import qualified Data.Aeson as A
import qualified Data.Text.Lazy as DTE
import qualified Data.Text.Lazy.Encoding as DTE
import qualified Environment
import JsonLogic
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Queries.ChakraQueries as SQCQ

mandatoryChakraFields :: [Text]
mandatoryChakraFields = ["userId"]

getChakraQueryFields :: LYT.Chakra -> Environment.Flow [Text]
getChakraQueryFields chakra = do
  queries <- SQCQ.findAllByChakra chakra
  return $ filter (\field -> field `notElem` mandatoryChakraFields) $ concatMap (.queryResults) queries

decodeTextToValue :: Text -> Either String Value
decodeTextToValue text =
  let byteString = DTE.encodeUtf8 $ DTE.fromStrict text
   in A.eitherDecode byteString

runJsonLogic :: (MonadFlow m) => Text -> Text -> m A.Value
runJsonLogic dataText ruleText = do
  let eitherRule = decodeTextToValue ruleText
  let eitherData = decodeTextToValue dataText
  rule <-
    case eitherRule of
      Right rule -> return rule
      Left err -> throwError $ InternalError ("Unable to decode rule:" <> show err)
  data' <-
    case eitherData of
      Right data_ -> return data_
      Left err -> throwError $ InternalError ("Unable to decode data:" <> show err)
  return $ jsonLogic rule data'
