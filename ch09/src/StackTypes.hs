{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module StackTypes
where
    
import LLM.OpenAI (ProviderData)

-- type that will hold our read-only configuration data
data Settings = Settings {
    llmProviders :: [ProviderData]
} deriving (Show)

-- type that will hold the State for our App
data AppState = AppState {}