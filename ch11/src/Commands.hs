{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Commands
where
import qualified Data.Text as T
import Control.Monad.RWS ( gets, MonadTrans(lift), MonadIO (liftIO), modify' )

import StackTypes
import Middleware (chatCompletionMid, Mid)
import LLM.OpenAI (userMessage)
import System.Console.Haskeline (outputStrLn, InputT)
import Util.PrettyPrinting (as, yellow, white, bold, blue, lblue, lgreen)
import Util.Formatting

import CMark

type App = InputT Mid

data CommandDescription = CommandDescription {
    helpTextShort :: String,
    helpTextLong :: String,
    commandName :: String,
    commandAction :: [String] -> App() -- [String] only includes options
}

allCommands::[CommandDescription]
allCommands = [
        CommandDescription {
            helpTextShort = ":send -- send a multiline message currently typed",
            helpTextLong = ":send -- send a multiline message currently typed",
            commandName = "send",
            commandAction = commandSendText
        },
        CommandDescription {
            helpTextShort = ":multi -- send a multiline message currently typed",
            helpTextLong = ":multi -- send a multiline message currently typed",
            commandName = "multi",
            commandAction = commandToggleMultiline
        }
    ]

processCommand :: [String] -> App()
processCommand ("help":_) = commandHelp
processCommand (cmd:opts) = do
    let c = filter (\x -> commandName x == cmd) allCommands
    if not (null c) then commandAction (head c) opts
    else controlMessage [white, bold] "No such command. Please try :help"
processCommand _ = controlMessage [white, bold] "No such command. Please try :help"

commandHelp :: App()
commandHelp = mapM_ (controlMessage [white,bold] . helpTextShort) allCommands

-- toggle multiline mode on or off
commandToggleMultiline :: [String] -> App()
commandToggleMultiline _ = do
    uis <- lift (gets uiState)
    lift $ modify' (\s -> s { uiState = uis {multilineMode = not (multilineMode uis)}})
    controlMessage [lgreen] ("[INFO] Multiline mode: " ++ show (not (multilineMode uis)))

-- send the text that is currently in the text buffer
commandSendText :: [String] -> App()
commandSendText _ = do
    txt <- currentLineBuffer <$> lift (gets uiState)
    if T.length txt > 0 then do
        (asMsg, _) <- lift (chatCompletionMid $ userMessage txt)
        uis <- lift (gets uiState)
        lift $ modify' (\s -> s { uiState = uis { currentLineBuffer = ""}})
        -- liftIO $ formatViaLatex (T.pack asMsg)
        let fmt = highlightCode "Markdown" (T.pack asMsg)
        outputStrLn (T.unpack fmt)
    else controlMessage [yellow] "[WARNING] Your message is empty, please type something first"


-- output ansified text message
controlMessage :: [String] -> String -> App()
controlMessage opts msg = outputStrLn (as opts msg)

-- checking for multiline mode
isMultilineOn :: App Bool
isMultilineOn = multilineMode <$> lift (gets uiState)
