{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module App
where

import qualified System.Console.Haskeline as HL
import qualified Util.PrettyPrinting as TC
import Control.Monad.RWS
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T
import LLM.OpenAI

import Middleware (Mid)
import StackTypes

type App = HL.InputT Mid

loop :: App ()
loop = do
  st <- lift get
  let mgr = httpManager st
  let modelId = currentModelId st
  let pr = currentProvider st
  minput <- HL.getInputLine  (TC.as [TC.bold] "\nλInt. ")
  case minput of
    Nothing -> return ()
    Just ":quit" -> return ()
    -- Just (':':cmds) -> (processCommand (words cmds)) >> loop
    Just input -> do
      -- outputStrLn $ "Input was: " ++ input
      liftIO $ chatCompletion mgr [userMessage $ T.pack input] modelId pr Nothing processResp
      loop