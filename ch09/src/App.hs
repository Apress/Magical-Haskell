{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module App
where

import Control.Monad.RWS
import StackTypes (Settings, AppState)

type App = RWST Settings [String] AppState IO