{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, ScopedTypeVariables #-}

module Init
(initAll)
where
    
import Configuration.Dotenv
import Configuration.Dotenv.Environment
import Data.ByteString.Char8 (pack)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import StackTypes
import LLM.OpenAI 
import Util.Logger (initLogger, LogLevels (DEBUG), initLoggerFile)
import Text.Read (readMaybe)

buildOpenAISettings :: IO ProviderData
buildOpenAISettings = do
    -- doing pattern match because we will have checked for presence before
    (Just key) <- lookupEnv "OPENAI_TOKEN"
    (Just url) <- lookupEnv "OPENAI_URL_CHAT"
    pure $ ProviderData {
    providerName = "openai",
    providerKey = pack key,
    chatCompletionURL = url,
    providerDefaultOptions = defaultChatOptions
}
    
initConfig :: IO Settings
initConfig = do
    loadFile defaultConfig
    oa <- buildOpenAISettings
    pure $ Settings { llmProviders = [oa], version="0.0.1"}

-- main initialization function in IO to create initial State and Settings
initAll :: IO (Settings, AppState)
initAll = do
    settings <- initConfig
    let prov = findProviderByName settings "openai"
    -- print prov
    manager <- newManager tlsManagerSettings
    lglev <- lookupEnv "LOG_LEVEL"
    let (Just lglev') :: Maybe LogLevels = maybe (Just DEBUG) readMaybe lglev
    lgState <- initLoggerFile lglev'
    let initSt = AppState manager "gpt-4o" prov lgState [] 6 (UIState True "")
    pure (settings, initSt)