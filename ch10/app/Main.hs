{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Init (initAll)
import System.Console.Haskeline
import App (loop)
import Control.Monad.RWS


main :: IO ()
main = do
    (sett, initSt) <- initAll
    (_, _, w) <- runRWST (runInputT (defaultSettings {historyFile = Just ".jarvis_history"}) loop) sett initSt
    putStrLn "Total usage:"
    print w
    putStrLn "Bye!"
