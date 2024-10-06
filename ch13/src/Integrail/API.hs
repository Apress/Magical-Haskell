{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Integrail.API
(
    IntegrailData(..),
    callAgentStreaming,
    processRespInt
)
where
-- import qualified Data.ByteString as BS
import qualified Data.Aeson as AE
import Data.Aeson ((.=), toJSON, fromJSON, ToJSON, FromJSON, (.:))
import qualified Data.Text as T
import Util.Logger (LoggerState, lg_dbg, lg_err)
import Network.HTTP.Client (Manager, responseStatus, parseRequest, method, responseHeaders, applyBearerAuth, Response (responseBody))
import Network.HTTP.Simple (setRequestBodyJSON, httpSink, setRequestHeader)
import Network.HTTP.Types (Status(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Conduit (foldMapMC)
import Data.ByteString.Char8 (unpack, ByteString, pack)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List.Split (splitOn)
import Control.Monad (when)

-- Define data types for the JSON operations
data Operation = Operation
  { op        :: String
  , nodeId    :: Maybe String
  , status    :: Maybe String
  , createdAt :: String
  , output    :: Maybe String
  , append    :: Maybe Bool
  , value     :: Maybe ValueType
  , stats     :: Maybe NodeStats
  } deriving (Show, Generic)

instance FromJSON Operation
instance ToJSON Operation

-- Define data types for the possible value types
data ValueType
  = ValueString String
  | ValueMessages [Message]
  deriving (Show, Generic)

instance FromJSON ValueType where
  parseJSON v@(AE.String _) = ValueString <$> AE.parseJSON v
  parseJSON v@(AE.Array _)  = ValueMessages <$> AE.parseJSON v

instance ToJSON ValueType

-- Define data types for messages
data Message = Message
  { role  :: String
  , parts :: [Part]
  } deriving (Show, Generic)

instance FromJSON Message
instance ToJSON Message

data Part = Part
  { type_ :: String
  , value_ :: String
  } deriving (Show, Generic)

instance FromJSON Part where
  parseJSON = AE.withObject "Part" $ \v -> Part <$>
    v .: "type" <*>
    v .: "value"

instance ToJSON Part where
  toJSON (Part type_ val) =
    AE.object ["type" .= type_, "value" .= val]

-- Define a data type for optional statistics
data NodeStats = NodeStats
  { inputTokens  :: Maybe Int
  , outputTokens :: Maybe Int
  , cost         :: Maybe Double
  } deriving (Show, Generic)

instance FromJSON NodeStats
instance ToJSON NodeStats

{- 


curl -X POST "https://cloud.integrail.ai/api/eXYr5zHyyWwPkgr6g/agent/N2jB2ZX35mDWXw8Z4/execute"             
        
-H "Content-Type: application/json"             
-d '{"inputs": {"inputs":{"userPrompt":"what is (2+2)*2"}}}'

{
  "inputs": {},
  "stream": true,
  "externalId": "string"
}

{"op":"init","execution":{"_id":"f27794b2-df54-464f-82eb-4e171aa7239c","status":"pending","updatedAt":"2024-10-06T19:45:36.489Z","queuedAt":"2024-10-06T19:45:36.489Z","pipelineId":"N2jB2ZX35mDWXw8Z4","pipeline":{"inputs":[{"type":"string","name":"userPrompt","saveHistory":false}],"outputs":[{"type":"string","name":"2-output","value":"{{2.output}}","saveHistory":true}],"nodes":[{"id":"2","name":"chatgpt-4o-latest:chat","inputs":[{"name":"messages","value":"{{3.messages}}"},{"name":"systemPrompt","value":"You are a helpful assistant named George. You are brilliant, fun, engaging, and love making jokes sometimes to keep the conversation flowing."},{"name":"temperature","value":0.8},{"name":"presencePenalty"},{"name":"maxToke
ns"},{"name":"topP"},{"name":"voiceChunks","value":false}]},{"id":"3","name":"chat-history","inputs":[{"name":"userPrompt","value":"{{inputs.userPrompt}}"},{"name":"limit","value":6}]}],"_id":"N2jB2ZX35mDWXw8Z4","version":2,"integrations":{},"accountId":"eXYr5zHyyWwPkgr6g"},"state":{"2":{"status":"pending","inputs":{},"outputs":{},"updatedAt":"2024-10-06T19:45:36.489Z","retries":0},"3":{"status":"pending","inputs":{},"outputs":{},"updatedAt":"2024-10-06T19:45:36.489Z","retries":0}},"inputs":{"userPrompt":"\nhi"},"outputs":{},"isLocked":false,"user":{"accountId":"eXYr5zHyyWwPkgr6g"}},"createdAt":"2024-10-06T19:45:36.489Z"}
{"op":"updateStatus","status":"running","createdAt":"2024-10-06T19:45:36.498Z"}

{"op":"node.updateStatus","nodeId":"3","status":"running","createdAt":"2024-10-06T19:45:36.525Z"}

{"op":"node.output.update","nodeId":"3","output":"messages","createdAt":"2024-10-06T19:45:36.551Z","status":"running","value":[{"role":"user","parts":[{"type":"string","value":"\nhi"}]}]}
{"op":"node.output.updateStatus","nodeId":"3","output":"messages","createdAt":"2024-10-06T19:45:36.553Z","status":"finished"}
{"op":"node.updateStatus","nodeId":"3","status":"finished","createdAt":"2024-10-06T19:45:36.553Z","stats":{}}

{"op":"node.updateStatus","nodeId":"2","status":"running","createdAt":"2024-10-06T19:45:36.582Z"}

{"op":"output.update","append":true,"output":"2-output","value":"","createdAt":"2024-10-06T19:45:38.171Z"}
{"op":"node.output.update","append":true,"nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.171Z","status":"running","value":""}
{"op":"output.update","append":true,"output":"2-output","value":"Hey","createdAt":"2024-10-06T19:45:38.172Z"}
{"op":"node.output.update","append":true,"nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.172Z","status":"running","value":"Hey"}

{"op":"output.update","append":true,"output":"2-output","value":" there","createdAt":"2024-10-06T19:45:38.237Z"}
{"op":"node.output.update","append":true,"nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.237Z","status":"running","value":" there"}
{"op":"output.update","append":true,"output":"2-output","value":"!","createdAt":"2024-10-06T19:45:38.238Z"}
{"op":"node.output.update","append":true,"nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.238Z","status":"running","value":"!"}

{"op":"output.update","append":true,"output":"2-output","value":" ð","createdAt":"2024-10-06T19:45:38.319Z"}
{"op":"node.output.update","append":true,"nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.319Z","status":"running","value":" ð"}
{"op":"output.update","append":true,"output":"2-output","value":" How","createdAt":"2024-10-06T19:45:38.319Z"}
{"op":"node.output.update","append":true,"nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.319Z","status":"running","value":" How"}

{"op":"output.update","append":true,"output":"2-output","value":"âs","createdAt":"2024-10-06T19:45:38.434Z"}
{"op":"node.output.update","append":true,"nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.434Z","status":"running","value":"âs"}
{"op":"output.update","append":true,"output":"2-output","value":" it","createdAt":"2024-10-06T19:45:38.435Z"}
{"op":"node.output.update","append":true,"nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.435Z","status":"running","value":" it"}
{"op":"output.update","append":true,"output":"2-output","value":" going","createdAt":"2024-10-06T19:45:38.435Z"}
{"op":"node.output.update","append":true,"nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.435Z","status":"running","value":" going"}
{"op":"output.update","append":true,"output":"2-output","value":"?","createdAt":"2024-10-06T19:45:38.436Z"}
{"op":"node.output.update","append":true,"nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.436Z","status":"running","value":"?"}
{"op":"output.update","append":true,"output":"2-output","value":"","createdAt":"2024-10-06T19:45:38.436Z"}
{"op":"node.output.update","append":true,"nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.436Z","status":"running","value":""}

{"op":"node.output.updateStatus","nodeId":"2","output":"voicedChunks","createdAt":"2024-10-06T19:45:38.438Z","status":"cancelled"}
{"op":"node.output.updateStatus","nodeId":"2","output":"output","createdAt":"2024-10-06T19:45:38.438Z","status":"finished"}
{"op":"node.output.update","nodeId":"2","output":"json","createdAt":"2024-10-06T19:45:38.443Z","status":"running","value":"Hey there! ð Howâs it going?"}
{"op":"node.output.updateStatus","nodeId":"2","output":"json","createdAt":"2024-10-06T19:45:38.443Z","status":"finished"}
{"op":"node.updateStatus","nodeId":"2","status":"finished","createdAt":"2024-10-06T19:45:38.449Z","stats":{"inputTokens":29,"outputTokens":10,"cost":0.000295}}

{"op":"updateStatus","status":"finished","createdAt":"2024-10-06T19:45:38.472Z"}

-}

data IntegrailData = IntegrailData {
    integrailKey :: ByteString,
    apiURL :: String
} deriving (Show)


callAgentStreaming :: IntegrailData -> AE.Value -> String -> LoggerState -> (ByteString -> IO String) -> IO String
callAgentStreaming ind inputs agentId lgState func = do
    -- Note: parseRequest_ throws an exception on invalid URLs, so in a real application, you should handle that.
    let url = apiURL ind ++ "agent/" ++ agentId ++ "/execute"
    let key = integrailKey ind
    lg_dbg ("Calling integrail agent with URL: " ++ url) lgState
    initialRequest <- parseRequest url
    let obj = AE.object [
                "stream" .= True,
                "inputs" .= inputs
            ]
    let request = setRequestBodyJSON obj $ applyBearerAuth key initialRequest
    let request' = setRequestHeader "Content-Type" ["application/json"] request
    let postRequest = request' { method = "POST" }

     -- Use withResponse to process the streaming response
    httpSink postRequest $ \response -> do
        let code = statusCode (responseStatus response)
        if (code /= 200) && (code /= 201)
            then do
                liftIO $ lg_err (show (responseStatus response)) lgState
                liftIO $ lg_err (show (responseHeaders response)) lgState
                liftIO $ lg_err (show (responseBody response)) lgState
                foldMapMC (\x -> do
                                  let y = unpack x
                                  putStr y
                                  pure y)
            else do
                -- Stream the response body to stdout
                v <- foldMapMC func
                liftIO $ lg_dbg ("Integrail response: " ++ show v) lgState
                pure v

processString :: String -> [String]
processString = splitOn "\n"


-- barebones function that shows everything -
-- take note, some chunks do not contain the whole json
processRespStupid :: ByteString -> IO String
processRespStupid ch = do
    print "======== NEW CHUNK ========"
    let st = unpack ch
    mapM_ (\x -> do
                    let jsonObject = AE.decode (BL.pack x) :: Maybe Operation
                    case jsonObject of
                        Nothing -> putStrLn "===== NOTHING ===== " >> putStrLn x >> putStrLn "==== END ===="
                        Just o -> print o
        ) (processString st)
    pure st

processRespInt :: ByteString -> IO String
processRespInt ch = do
    let st = unpack ch
    mapM_ (\x -> do
                    let jsonObject = AE.decode (BL.pack x) :: Maybe Operation
                    case jsonObject of
                        Nothing -> pure ()
                        Just o ->
                          case op o of
                            "node.updateStatus" -> pure () -- print $ stats o
                            "output.update" -> when (append o == Just True) $ appx $ value o
                            _ -> pure ()
        ) (processString st)
    pure st

appx (Just (ValueString s)) = putStr s
appx _ = pure ()