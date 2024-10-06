{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module StackTypes
where
    
import LLM.OpenAI (ProviderData(..), Message)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Util.Logger (LoggerState)
import Mongo.Core (MongoState)
import VectorStorage.InMemory (MemoryStorage)
import Integrail.API (IntegrailData)

-- type that will hold our read-only configuration data
data Settings = Settings {
    llmProviders :: [ProviderData],
    version :: String,
    mongoSettings :: MongoState,
    integrailSettings :: Maybe IntegrailData
} 

-- various UI settings
data UIState = UIState {
    multilineMode :: Bool, -- multiline entry mode
    currentLineBuffer :: Text -- current buffer if in the multiline mode
} deriving (Show)

-- type that will hold the State for our App
data AppState = AppState {
    httpManager :: Manager,
    currentModelId :: Text,
    currentProvider :: ProviderData,
    loggerState :: LoggerState,
    messageHistory :: [Message],
    historySize :: Int,
    uiState :: UIState,
    memoryStore :: MemoryStorage
} 

-- returns provider data (url, secrets etc) by its name
findProviderByName :: Settings -> Text -> ProviderData
-- default is OpenAI
findProviderByName st _ = head (llmProviders st)
