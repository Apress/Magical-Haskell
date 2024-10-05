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
import Util.Logger (initLogger, LogLevels (DEBUG), initLoggerFile, lg_err, lg_suc)
import Text.Read (readMaybe)
import Mongo.Core (initMongo, MongoState (mainConnection))
import Mongo.MongoRAG (findAllRAG)
import qualified Data.Vector as V


buildOpenAISettings :: IO ProviderData
buildOpenAISettings = do
    -- doing pattern match because we will have checked for presence before
    (Just key) <- lookupEnv "OPENAI_TOKEN"
    (Just url) <- lookupEnv "OPENAI_URL_CHAT"
    (Just url1)<- lookupEnv "OPENAI_URL_EMBEDDINGS"
    pure $ ProviderData {
    providerName = "openai",
    providerKey = pack key,
    chatCompletionURL = url,
    providerDefaultOptions = defaultChatOptions,
    embeddingsURL = url1
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
    checkEnvironment lgState
    mongoState <- initMongo lgState
    memst <- findAllRAG $ mainConnection mongoState
    let settings' = settings { mongoSettings = mongoState}
    let initSt = AppState manager "gpt-4o" prov lgState [] 6 (UIState True "") (V.fromList memst)
    pure (settings', initSt)

checkEnvironmentVar lgs var = do
    e1 <- lookupEnv var
    maybe (lg_err ("Environment variable missing: " ++ var) lgs)
          (\x -> do lg_suc ("Loaded environment: " ++ var) lgs)
          e1

checkEnvironment lgs = mapM_ (checkEnvironmentVar lgs) [
        "OPENAI_ORG",
        "OPENAI_TOKEN",
        "OPENAI_URL_CHAT",
        "OPENAI_URL_MODELS",
        "TEST_ERROR",
        "OPENAI_URL_EMBEDDINGS",
        "MAIN_MONGO_URI"
    ]
