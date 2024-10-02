{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Init (initAll)
import System.Console.Haskeline
import App (loop)
import Control.Monad.RWS
import Middleware (lgInf)
import Util.PrettyPrinting (as, white, bold, yellow, lgreen)


main :: IO ()
main = do
    (sett, initSt) <- initAll
    evalRWST (lgInf "starting up Jarvis") sett initSt
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
    putStrLn $ "In the multiline mode, type freely and then on the new line type " ++ as [yellow, bold] ":send" ++ "to send the message"
    putStrLn $ "In the single-line mode, every time you hit " ++ as [lgreen, bold] "'Enter'" ++ " the message will be sent"
    putStrLn "Enjoy!"
    putStrLn ""