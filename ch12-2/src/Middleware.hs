{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Middleware
where

import StackTypes (Settings, AppState (AppState, loggerState, currentModelId), findProviderByName, currentProvider, messageHistory)
import Util.Logger
import LLM.OpenAI (Usage, chatCompletion, Message, processResp, providerDefaultOptions, assistantMessage, embedText, embeddingModels, embeddingName)
import Data.Text (pack, Text)
import Util.PrettyPrinting (as, white, bold, lgreen)
import Control.Monad.MRWS
import Control.Monad.RWS (lift, liftIO)

-- monad that handles all application's business logic
type Mid = MRWST Settings Usage AppState IO

-- openai ---------------------------------
embedTextMid :: Text -> Mid ()
embedTextMid txt = do
    st <- get
    let prov = currentProvider st
    let mdlName = embeddingName (head embeddingModels)
    lift $ embedText txt mdlName prov (loggerState st)

chatCompletionMid :: Message -> Mid (String, Usage)
chatCompletionMid message = do
    st <- get
    let provider = currentProvider st
    let msgs = messageHistory st
    let messages = msgs ++ [message]
    liftIO (putStrLn "" >> putStrLn (as [white,bold] "[Jarvis]"))
    (asMsg, us) <- lift $ chatCompletion messages (currentModelId st) provider Nothing (loggerState st) processResp
    let messages' = messages ++ [assistantMessage $ pack asMsg]
    modify (\s -> s {messageHistory = messages'})
    tell us
    pure (asMsg, us)
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

