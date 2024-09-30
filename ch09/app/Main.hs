{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Init (initConfig)
import StackTypes (findProviderByName)

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import LLM.OpenAI (chatCompletion, userMessage, testChunkStreaming)


main :: IO ()
main = do
    settings <- initConfig
    let prov = findProviderByName settings "openai"
    print prov
    manager <- newManager tlsManagerSettings
    chatCompletion manager [userMessage "hi"] "gpt-4o" prov Nothing testChunkStreaming
