{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Init
where
    
import Configuration.Dotenv
import Configuration.Dotenv.Environment
import Data.ByteString.Char8 (pack)

import StackTypes
import LLM.OpenAI 


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