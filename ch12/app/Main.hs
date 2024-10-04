{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Init (initAll)
import System.Console.Haskeline
import App (loop)
import Control.Monad.RWS
import Middleware (lgInf)
import Util.PrettyPrinting (as, white, bold, yellow, lgreen)

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Maybe (fromJust)
import WebAPI.WebApp
import Control.Concurrent (forkOS, ThreadId)
import Mongo.SystemPrompts (insertSPD)
import StackTypes (Settings(..))
import Mongo.Core (MongoState(..))


main :: IO ()
main = do
    (sett, initSt) <- initAll
    wid <- forkOS $ mainServer 8080 sett initSt
    evalRWST (lgInf "starting up Jarvis") sett initSt
    -- res <- insertSPD (mainConnection $ mongoSettings sett) "Jarvis" "You are a the best in the world Haskell developer. You enjoy sharing your knowledge."
    -- print res
    showHeader
    putStrLn "" >> putStrLn (as [white,bold] "[User]")
    (_, _, w) <- runRWST (runInputT (defaultSettings {historyFile = Just ".jarvis_history"}) loop) sett initSt
    putStrLn "Total usage:"
    print w
    putStrLn "Bye!"

showHeader :: IO ()
showHeader = do
    putStrLn (as [white,bold] "Welcome to the Jarvis AI System!")
    putStrLn "Version 1.0.0"
    putStrLn ""
    putStrLn $ "For help, type " ++ as [yellow, bold] ":help"
    putStrLn $ "In the multiline mode, type freely and then on the new line type " ++ as [yellow, bold] ":send" ++ " to send the message"
    putStrLn $ "In the single-line mode, every time you hit " ++ as [lgreen, bold] "'Enter'" ++ " the message will be sent"
    putStrLn "Enjoy!"
    putStrLn ""