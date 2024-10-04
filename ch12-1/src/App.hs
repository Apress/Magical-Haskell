{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module App
where

import qualified System.Console.Haskeline as HL
import qualified Util.PrettyPrinting as TC
import Control.Monad.RWS ( gets, MonadTrans(lift), modify' )
import qualified Data.Text as T
import LLM.OpenAI

import Middleware (Mid, lgInf, chatCompletionMid)
import StackTypes
import Commands (processCommand, App, isMultilineOn, controlMessage)


loop :: App ()
loop = do
  ml <- isMultilineOn
  if ml then HL.getInputLine "" >>= processInput
  else HL.getInputLine  (TC.as [TC.bold] "\n[User]\n") >>= processInput
  where 
    processInput minput = do
      ml <- isMultilineOn
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just (':':cmds) -> processCommand (words cmds) >> controlMessage [TC.bold] "[User]" >> loop
        Just input ->
          if ml then do
            uis <- lift (gets uiState)
            lift $ modify' (\s -> s { uiState = uis { currentLineBuffer = currentLineBuffer uis `T.append` "\n" `T.append` T.pack input}})
            loop
          else lift (chatCompletionMid $ userMessage $ T.pack input) >> loop
