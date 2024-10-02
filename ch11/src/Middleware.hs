{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Middleware
where

import Control.Monad.RWS
import StackTypes (Settings, AppState (AppState, loggerState, currentModelId), findProviderByName, currentProvider, messageHistory)
import Util.Logger
import LLM.OpenAI (Usage, chatCompletion, Message, processResp, providerDefaultOptions, assistantMessage)
import Data.Text (pack)
import Util.PrettyPrinting (as, white, bold, lgreen)

-- monad that handles all application's business logic
type Mid = RWST Settings Usage AppState IO

-- openai ---------------------------------

chatCompletionMid :: Message -> Mid ()
chatCompletionMid message = do
    st <- get
    let provider = currentProvider st
    let msgs = messageHistory st
    let messages = msgs ++ [message]
    liftIO (putStrLn "" >> putStrLn (as [white,bold] "[Jarvis]"))
    (asMsg, us) <- lift $ chatCompletion messages (currentModelId st) provider Nothing (loggerState st) processResp
    let messages' = messages ++ [assistantMessage $ pack asMsg]
    modify' (\s -> s {messageHistory = messages'})
    tell us
    -- liftIO (putStrLn "" >> putStrLn (as [lgreen,bold] "[DONE]"))
    -- liftIO (putStrLn "" >> putStrLn (as [white,bold] "[User]"))
    
                

-- logging --------------------------------
lgL :: LogLevels -> String -> Mid ()
lgL lvl msg = gets loggerState >>= liftIO . lgl lvl msg

lgFtl :: [Char] -> Mid ()
lgFtl msg = gets loggerState >>= liftIO . lg_ftl msg
lgErr :: [Char] -> Mid ()
lgErr msg = gets loggerState >>= liftIO . lg_err msg
lgWrn :: [Char] -> Mid ()
lgWrn msg = gets loggerState >>= liftIO . lg_wrn msg
lgInf :: [Char] -> Mid ()
lgInf msg = gets loggerState >>= liftIO . lg_inf msg
lgSuc :: [Char] -> Mid ()
lgSuc msg = gets loggerState >>= liftIO . lg_suc msg
lgDbg :: [Char] -> Mid ()
lgDbg msg = gets loggerState >>= liftIO . lg_dbg msg
lgVrb :: [Char] -> Mid ()
lgVrb msg = gets loggerState >>= liftIO . lg_vrb msg

