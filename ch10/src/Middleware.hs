{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Middleware
where

import Control.Monad.RWS
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import StackTypes (Settings, AppState (AppState), findProviderByName)
import Init (initConfig)

-- monad that handles all application's business logic
type Mid = RWST Settings [String] AppState IO

init :: Mid ()
init = do
    settings <- liftIO initConfig
    let prov = findProviderByName settings "openai"
    liftIO $ print prov
    manager <- liftIO $ newManager tlsManagerSettings
    let initSt = AppState manager "gpt-4o" prov
    put initSt


