{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Init (initAll)
import StackTypes (findProviderByName)

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import LLM.OpenAI (chatCompletion, userMessage, testChunkStreaming, processResp)
import System.Console.Haskeline
import App (loop)
import Control.Monad.RWS


main :: IO ()
main = do
    (sett, initSt) <- initAll
    runRWST (runInputT (defaultSettings {historyFile = Just ".jarvis_history"}) loop) sett initSt
    putStrLn "bye!"
