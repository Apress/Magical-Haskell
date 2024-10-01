{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module LLM.OpenAI
(
  chatCompletion, 
  userMessage, 
  testChunkStreaming, 
  processResp,
  ProviderData(..),
  defaultChatOptions)
where
  

import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=), decode, FromJSON, ToJSON, toJSON, Value, Object)
import Data.Aeson.Types (Value(..))
import Data.Text (Text)
import Network.HTTP.Client (Manager, responseStatus, parseRequest, method, responseHeaders, applyBearerAuth)
import Network.HTTP.Simple (setRequestBodyJSON, httpSink)
import Network.HTTP.Types (statusCode)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as BS
import Data.List.Split ( splitOn )
import Data.Maybe (fromMaybe)
import Conduit (mapM_C)
import qualified Data.Text as T

-- Ok since we are using OpenAI api as primary, llm data is (at least for now) stored here:
data ProviderData = ProviderData {
    providerName :: Text,
    providerKey :: BS.ByteString, -- with Bearer for now to be faster
    chatCompletionURL :: String,
    providerDefaultOptions :: ChatOptions
} deriving (Show)

data LLMDescription = LLMDescription {
    llmIndex :: Int,
    provider :: Text,
    modelName :: Text
} deriving (Show)

availableLLMs :: [LLMDescription]
availableLLMs = [
    LLMDescription 1 "openai" "gpt-4o",
    LLMDescription 2 "openai" "gpt-4o-2024-05-13"
    ]

{- 
TBD:
messages=[
    {
      "role": "user",
      "content": [
        {
          "type": "text",
          "text": "What are in these images? Is there any difference between them?",
        },
        {
          "type": "image_url",
          "image_url": {
            "url": "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/Gfp-wisconsin-madison-the-nature-boardwalk.jpg/2560px-Gfp-wisconsin-madison-the-nature-boardwalk.jpg",
          },
        },
        {
          "type": "image_url",
          "image_url": {
            "url": "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/Gfp-wisconsin-madison-the-nature-boardwalk.jpg/2560px-Gfp-wisconsin-madison-the-nature-boardwalk.jpg",
          },
        },
      ],
    }
  ],
-}
data ContentWithImages = ContentWithImages {

}

data Message = Message
  { role :: Text -- "user" | "assistant" | "system" - need to change to proper types
  , content :: Text
  , tool_calls :: Maybe [Text]
  , tool_call_id :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message

userMessage :: Text -> Message
userMessage msg = Message "user" msg Nothing Nothing
assistantMessage :: Text -> Message
assistantMessage msg = Message "assistant" msg Nothing Nothing
systemMessage :: Text -> Message
systemMessage msg = Message "system" msg Nothing Nothing

data DeltaMessage = DeltaMessage
  {
    content :: Text
  , tool_calls :: Maybe [Text]
  } deriving (Show, Generic)

instance ToJSON DeltaMessage
instance FromJSON DeltaMessage


data Choice = Choice
  { message :: Maybe Message
  , delta :: Maybe DeltaMessage
  , index :: Maybe Int
  , finish_reason :: Maybe Text
  , logprobs :: Maybe [Text]
  } deriving (Show, Generic)

instance ToJSON Choice
instance FromJSON Choice

data Usage = Usage
  { prompt_tokens :: Int
  , completion_tokens :: Int
  , total_tokens :: Int
  } deriving (Show, Generic)

instance ToJSON Usage
instance FromJSON Usage

data OpenAIResponse = OpenAIResponse
  { id :: Text
  , object :: Text
  , created :: Int
  , model :: Text
  , choices :: [Choice]
  , usage :: Maybe Usage
  } deriving (Show, Generic)

instance ToJSON OpenAIResponse
instance FromJSON OpenAIResponse

data OpenAIStreamOptions = OpenAIStreamOptions {
  include_usage :: Bool
} deriving (Show, Generic)

instance ToJSON OpenAIStreamOptions
instance FromJSON OpenAIStreamOptions

data ChatOptions = ChatOptions {
    stream :: Bool,
    {- 
    Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim. Default: null
    -}
    frequency_penalty :: Maybe Float,
    {- Whether to return log probabilities of the output tokens or not. If true, returns the log probabilities of each output token returned in the content of message. Default: False -}
    logprobs :: Maybe Bool,
    {- An integer between 0 and 20 specifying the number of most likely tokens to return at each token position, each with an associated log probability. logprobs must be set to true if this parameter is used. -}
    top_logprobs :: Maybe Int,
    {- The maximum number of tokens that can be generated in the chat completion.The total length of input tokens and generated tokens is limited by the model's context length. -}
    max_tokens :: Maybe Int,
    {- Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics. -}
    presence_penalty :: Maybe Float,
    {- An object specifying the format that the model must output. Compatible with GPT-4 Turbo and all GPT-3.5 Turbo models newer than gpt-3.5-turbo-1106.Setting to { "type": "json_object" } enables JSON mode, which guarantees the message the model generates is valid JSON. 
    Important: when using JSON mode, you must also instruct the model to produce JSON yourself via a system or user message. Without this, the model may generate an unending stream of whitespace until the generation reaches the token limit, resulting in a long-running and seemingly "stuck" request. Also note that the message content may be partially cut off if finish_reason="length", which indicates the generation exceeded max_tokens or the conversation exceeded the max context length. -}
    response_format :: Maybe Object,
    {- This feature is in Beta. If specified, our system will make a best effort to sample deterministically, such that repeated requests with the same seed and parameters should return the same result. Determinism is not guaranteed, and you should refer to the system_fingerprint response parameter to monitor changes in the backend. -}
    seed :: Maybe Int,
    {- Optional Defaults to null 
    Up to 4 sequences where the API will stop generating further tokens.-}
    stop :: Maybe [Text],
    {- Options for streaming response. Only set this when you set stream: true.
    Optional
    If set, an additional chunk will be streamed before the data: [DONE] message. The usage field on this chunk shows the token usage statistics for the entire request, and the choices field will always be an empty array. All other chunks will also include a usage field, but with a null value. -}
    stream_options :: Maybe OpenAIStreamOptions,
    {- What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic.
    We generally recommend altering this or top_p but not both. Defaults to 1 -}
    temperature :: Maybe Float,
    {- Optional
    Defaults to 1
    An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered.
    We generally recommend altering this or temperature but not both. -}
    top_p :: Maybe Float,
    {- A list of tools the model may call. Currently, only functions are supported as a tool. Use this to provide a list of functions the model may generate JSON inputs for. A max of 128 functions are supported. -}
    tools :: Maybe [Object],
    {- Controls which (if any) tool is called by the model. none means the model will not call any tool and instead generates a message. auto means the model can pick between generating a message or calling one or more tools. required means the model must call one or more tools. Specifying a particular tool via {"type": "function", "function": {"name": "my_function"}} forces the model to call that tool.
        none is the default when no tools are present. auto is the default if tools are present. -}
    tool_choice :: Maybe Text,
    {- A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.  -}
    user :: Maybe Text
} deriving (Show, Generic)

instance ToJSON ChatOptions
instance FromJSON ChatOptions

defaultChatOptions :: ChatOptions
defaultChatOptions = ChatOptions {
    stream = True,
    frequency_penalty = Nothing,
    logprobs = Nothing,
    top_logprobs = Nothing,
    max_tokens = Nothing,
    presence_penalty = Nothing,
    response_format = Nothing,
    seed = Nothing,
    stop = Nothing,
    stream_options = Just (OpenAIStreamOptions True),
    temperature = Just 1,
    top_p = Nothing,
    tools = Nothing,
    tool_choice = Nothing,
    user = Nothing
}

-- Now request for chat completion

-- Function to combine two JSON objects
combineObjects :: Value -> Value -> Value
combineObjects (Object obj1) (Object obj2) = Object (obj1 <> obj2)
combineObjects _ _ = error "Both inputs must be JSON objects"

chatCompletion :: Manager -> [Message] -> Text -> ProviderData -> Maybe ChatOptions -> (BS.ByteString -> IO()) -> IO ()
chatCompletion mgr messages modelId provider opts func = do
    -- Note: parseRequest_ throws an exception on invalid URLs, so in a real application, you should handle that.
    let url = chatCompletionURL provider
    let key = providerKey provider
    let fopts = Data.Maybe.fromMaybe (providerDefaultOptions provider) opts
    -- putStrLn $ "Calling chatCompletion with URL: " ++ url ++ " and key " ++ (show key)
    initialRequest <- parseRequest url
    let obj = Data.Aeson.object [
                "model" .= modelId,
                "messages" .= toJSON messages
            ]
    let request = setRequestBodyJSON (combineObjects obj (toJSON fopts))
            $ applyBearerAuth key initialRequest

    let postRequest = request { method = "POST" }

     -- Use withResponse to process the streaming response
    httpSink postRequest $ \response -> do
        if statusCode (responseStatus response) /= 200
            then do
                liftIO $ print (responseStatus response)
                liftIO $ print (responseHeaders response)
                -- putStrLn $ show (responseBody response) 
                mapM_C (putStrLn . unpack)
            else do
                -- Stream the response body to stdout
                mapM_C func

testChunk :: BS.ByteString -> IO ()
testChunk ch = do
    putStrLn $ unpack ch
    putStrLn "\n"
    let jsonObject = decode (BL.pack (unpack ch)) :: Maybe OpenAIResponse
    print jsonObject
    -- putStrLn "\n\n"

testChunkStreaming :: BS.ByteString -> IO ()
testChunkStreaming ch = do
    let ttt = unpack ch
    let objs = processString ttt
    mapM_ (\x -> do
        putStrLn x
        putStrLn $ "Length is: " ++ show (length x)
        let jsonObject = decode (BL.pack x) :: Maybe OpenAIResponse
        print jsonObject)
        objs
    --putStrLn ttt
    --putStrLn $ "Length is: " ++ show (length ttt)

    -- let jsonObject = decode (BL.pack ttt) :: Maybe OpenAIResponse
    -- print jsonObject
    -- putStrLn "\n\n"

processString :: String -> [String]
processString = splitOn "data: "

processResp :: BS.ByteString -> IO ()
processResp ch = do
  mapM_
    ( \x -> do
        -- putStrLn x
        -- putStrLn $ "Length is: " ++ show (length x)
        let jsonObject = decode (BL.pack x) :: Maybe OpenAIResponse
        case jsonObject of
          Nothing -> return ()
          Just resp -> do
            let chs = delta $ head $ choices resp
            -- dlt <- content <$> (chs :: Maybe DeltaMessage)
            case chs of
              Nothing -> return ()
              Just DeltaMessage {content = cnt} -> do
                putStr (T.unpack cnt)
    )
    (processString $ unpack ch)