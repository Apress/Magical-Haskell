{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Util.Logger
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.RWS (lift)
import Control.Monad.State.Lazy (StateT, put, get, MonadState, evalStateT, modify')
import System.Log.FastLogger
import Data.Monoid ((<>))
import Data.Text
import qualified Util.PrettyPrinting as TC

data LogLevels = FATAL | ERROR | WARNING | SUCCESS | INFO | DEBUG | VERBOSE deriving (Show, Eq, Ord, Read)

data LoggerState = LoggerState {
    logger::TimedFastLogger,
    cleanUpLogger:: IO(),
    level :: LogLevels
}

initLogger :: LogLevels -> IO LoggerState
initLogger lvl = do
    timeCache <- newTimeCache simpleTimeFormat
    (logger, cleanUp) <- newTimedFastLogger timeCache (LogStdout defaultBufSize)
    let st = LoggerState logger cleanUp lvl
    return st

initLoggerFile :: LogLevels -> IO LoggerState
initLoggerFile lvl = do
    timeCache <- newTimeCache simpleTimeFormat
    (logger, cleanUp) <- newTimedFastLogger timeCache (LogFileNoRotate ".jarvis_logs" defaultBufSize)
    let st = LoggerState logger cleanUp lvl
    return st

lg :: LogLevels -> [Char] -> [String] -> LoggerState -> IO ()
lg lvl msg clr lstate = do
    let st = level lstate
    let lgr = logger lstate
    when (lvl <= st) $ lgr $ \ft -> toLogStr ("["::Text) <> toLogStr ft <> toLogStr ("]"::Text) <> toLogStr (TC.as clr $ "["++show lvl++"]" ++ msg ++ "\n")

lgPlainLvl :: LogLevels -> [Char] -> LoggerState -> IO ()
lgPlainLvl lvl msg lstate = do
    let st = level lstate
    let lgr = logger lstate
    when (lvl <= st) $ lgr $ \ft -> toLogStr ("["::Text) <> toLogStr ft <> toLogStr ("]"::Text) <> toLogStr ("\n" ++ msg ++ "\n")

lgPlain :: [Char] -> LoggerState -> IO ()
lgPlain = lgPlainLvl VERBOSE

lg_ftl :: [Char] -> LoggerState -> IO ()
lg_ftl msg = lg  FATAL msg [TC.red]
lg_err :: [Char] -> LoggerState -> IO ()
lg_err msg = lg  ERROR msg [TC.lred]
lg_wrn :: [Char] -> LoggerState -> IO ()
lg_wrn msg = lg  WARNING msg [TC.yellow]
lg_inf :: [Char] -> LoggerState -> IO ()
lg_inf msg = lg  INFO msg [TC.lblue]
lg_suc :: [Char] -> LoggerState -> IO ()
lg_suc msg = lg  SUCCESS msg [TC.green]
lg_dbg :: [Char] -> LoggerState -> IO ()
lg_dbg msg = lg  DEBUG msg [TC.lgray]
lg_vrb :: [Char] -> LoggerState -> IO ()
lg_vrb msg = lg  VERBOSE msg [TC.white]

lgl :: LogLevels -> String -> LoggerState -> IO ()
lgl lvl msg = lg lvl msg (getLogClr lvl)

getLogClr :: LogLevels -> [String]
getLogClr FATAL = [TC.red]
getLogClr ERROR = [TC.lred]
getLogClr WARNING = [TC.yellow]
getLogClr INFO = [TC.lblue]
getLogClr SUCCESS = [TC.green]
getLogClr DEBUG = [TC.lgray]
getLogClr VERBOSE = [TC.white]

testLogs :: LoggerState -> IO ()
testLogs st = do
    lg_ftl  "This is a fatal error message" st
    lg_err  "This is a normal error message" st
    lg_wrn  "This is a warning message" st
    lg_inf  "This is a info message" st
    lg_suc  "This is a success message" st
    lg_dbg  "This is a debug message" st
    lg_vrb  "This is a verbose message" st
