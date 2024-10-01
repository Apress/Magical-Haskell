{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Middleware
where

import Control.Monad.RWS

import StackTypes (Settings, AppState (AppState), findProviderByName)


-- monad that handles all application's business logic
type Mid = RWST Settings [String] AppState IO



