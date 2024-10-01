{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module App
where

import qualified System.Console.Haskeline as HL
import qualified Util.PrettyPrinting as TC
import Control.Monad.RWS
import qualified Data.Text as T
import LLM.OpenAI

import Middleware (Mid, lgInf, chatCompletionMid)
import StackTypes

type App = HL.InputT Mid

loop :: App ()
loop = do
  lift $ lgInf "starting up Jarvis"
  minput <- HL.getInputLine  (TC.as [TC.bold] "\nλInt. ")
  case minput of
    Nothing -> return ()
    Just ":quit" -> return ()
    -- Just (':':cmds) -> (processCommand (words cmds)) >> loop
    Just input -> lift (chatCompletionMid $ userMessage $ T.pack input) >> loop

