{-#LANGUAGE OverloadedStrings #-}

module GameFloor
where

import Control.Monad.State
import Cards
import IOFloor

-- Our monad transformer stack
type Game = StateT GameState IO

initialState :: GameState
initialState = GameState
    { player = Player [] 0 1000
    , dealer = Dealer []
    , deck = fullDeck
    }

playerBetAction :: Game ()
playerBetAction = do
    bet <- liftIO getPlayerBet
    pl <- gets player
    let pl' = makeBet bet pl
    modify (\s -> s { player = pl'})

dealCardsToPlayer :: Int -> Game ()
dealCardsToPlayer n = do
    -- read current GameState
    gs <- get
    let pl = player gs
    -- deal cards using pure function
    let (newDeck, playerDealt) = dealCards n (deck gs)
    -- create new player status with new cards
    let pl' = pl { playerHand = (playerHand pl) ++ playerDealt }
    -- update game status
    modify (\s -> s { player = pl', deck = newDeck})

dealCardsToDealer :: Int -> Game ()
dealCardsToDealer n = do
    -- read current GameState
    gs <- get
    let dl = dealer gs
    -- deal cards using pure function
    let (newDeck, playerDealt) = dealCards n (deck gs)
    -- create new player status with new cards
    let dl' = dl { dealerHand = (dealerHand dl) ++ playerDealt }
    -- update game status
    modify (\s -> s { dealer = dl', deck = newDeck})

showPlayerState :: Game ()
showPlayerState = do
    pl <- gets player
    liftIO $ print pl

showDealerHand :: Game ()
showDealerHand = do
    dl <- gets dealer
    liftIO $ print dl

dealerAction :: Int -> Game ()
dealerAction pvalue = do
    dl <- gets dealer
    let value = handValue (dealerHand dl)
    if value < 17
        then do
            dealCardsToDealer 1
            showDealerHand
            dealerAction pvalue
        else do
            showDealerHand
            liftIO $ putStrLn $ "Dealer stands with " ++ show value
            when ((value > 21) || (value < pvalue)) $ do 
                liftIO $ putStrLn "Player wins!"
                pl <- gets player
                let pl' = payout ((playerBet pl)*3) pl
                modify (\s -> s {player = pl'})
                showPlayerState

playerAction :: Game ()
playerAction = do
    showDealerHand
    showPlayerState
    action <- liftIO $ do
        putStrLn "Do you want to (H)it or (S)tand?"
        getLine
    case action of
        "H" -> do
            dealCardsToPlayer 1
            pl <- gets player
            let newValue = handValue (playerHand pl)
            if newValue > 21
                then do
                    liftIO $ putStrLn "You bust!"
                    let pl' = payout 0 pl
                    modify (\s -> s {player = pl'})
                    showPlayerState
                    -- modifyPlayer player (\p -> p { playerMoney = playerMoney p - playerBet p })
                else if newValue == 21
                    then do
                        liftIO $ putStrLn "21! Paying out 2x!"
                        let pl' = payout ((playerBet pl)*3) pl
                        modify (\s -> s {player = pl'})
                        showPlayerState
                    else playerAction
        "S" -> do
            showPlayerState
            liftIO $ putStrLn "You stand"
            pl <- gets player
            let newValue = handValue (playerHand pl)
            dealerAction newValue
        _ -> do
            liftIO $ putStrLn "Invalid action. Please choose H or S."
            playerAction