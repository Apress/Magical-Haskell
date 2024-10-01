{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module StackTypes
where
    
import LLM.OpenAI (ProviderData(..))
import Data.Text (Text)
import Network.HTTP.Client (Manager)

-- type that will hold our read-only configuration data
data Settings = Settings {
    llmProviders :: [ProviderData],
    version :: String
} deriving (Show)

-- type that will hold the State for our App
data AppState = AppState {
    httpManager :: Manager,
    currentModelId :: Text,
    currentProvider :: ProviderData
}

-- returns provider data (url, secrets etc) by its name
findProviderByName :: Settings -> Text -> ProviderData
-- default is OpenAI
findProviderByName st _ = head (llmProviders st)
