{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.IO.Class
import Data.List (sortOn)
import qualified Data.Text as T
import System.Random
import Control.Monad (when, unless, forM)
import Text.Read (readMaybe)

-- Let's start with our basic types

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)
data Card = Card { rank :: Rank, suit :: Suit } deriving (Show, Eq)

type Deck = [Card]
type Hand = [Card]

data Player = Player { playerName :: String, playerHand :: Hand, playerBet :: Int, playerMoney :: Int } deriving Show
data Dealer = Dealer { dealerHand :: Hand } deriving Show

data GameState = GameState
    { players :: [Player]
    , dealer :: Dealer
    , deck :: Deck
    } deriving Show

data GameConfig = GameConfig
    { initialMoney :: Int
    , minBet :: Int
    , maxBet :: Int
    } deriving Show

-- Our monad transformer stack
type Game a = StateT GameState (ReaderT GameConfig (WriterT [String] IO)) a

-- Helper functions

shuffleDeck :: Deck -> IO Deck
shuffleDeck deck = do
    gen <- newStdGen
    return $ map snd $ sortOn fst $ zip (randoms gen :: [Int]) deck

fullDeck :: Deck
fullDeck = [Card r s | r <- [Two .. Ace], s <- [Hearts, Diamonds, Clubs, Spades]]

cardValue :: Card -> Int
cardValue (Card rank _) = case rank of
    Ace -> 11  -- We'll handle Ace's 1 or 11 value in handValue
    Jack -> 10
    Queen -> 10
    King -> 10
    _ -> fromEnum rank - fromEnum Two + 2

handValue :: Hand -> Int
handValue hand =
    let baseValue = sum $ map cardValue hand
        aces = length $ filter (\c -> rank c == Ace) hand
    in if baseValue > 21 && aces > 0
       then handValue $ map (\c -> if rank c == Ace then Card Two (suit c) else c) hand
       else baseValue

-- Game logic

dealCard :: Game Card
dealCard = do
    gs <- get
    case deck gs of
        (card:rest) -> do
            put gs { deck = rest }
            logAction $ "Dealt: " ++ show card
            return card
        [] -> error "Deck is empty!"

dealInitialCards :: Game ()
dealInitialCards = do
    mapM_ (\p -> do
        card1 <- dealCard
        card2 <- dealCard
        modifyPlayer p (\pl -> pl { playerHand = [card1, card2] })
        showHand p
        ) =<< gets players
    dealerCard <- dealCard
    modifyDealer (\d -> d { dealerHand = [dealerCard] })
    showDealerInitialHand

modifyPlayer :: Player -> (Player -> Player) -> Game ()
modifyPlayer player f = modify (\gs -> gs { players = map (\p -> if playerName p == playerName player then f p else p) (players gs) })

modifyDealer :: (Dealer -> Dealer) -> Game ()
modifyDealer f = modify (\gs -> gs { dealer = f (dealer gs) })

playerTurn :: Player -> Game ()
playerTurn player = do
    logAction $ playerName player ++ "'s turn"
    if handValue (playerHand player) == 21
        then do
            showHand player
            logAction $ playerName player ++ " has Blackjack!"
            modifyPlayer player (\p -> p { playerMoney = playerMoney p + (playerBet p * 3 `div` 2) })
        else playerAction player
        
playerAction :: Player -> Game ()
playerAction player = do
    showHand player
    showDealerInitialHand
    action <- liftIO $ do
        putStrLn $ playerName player ++ ", do you want to (H)it or (S)tand?"
        getLine
    case action of
        "H" -> do
            card <- dealCard
            modifyPlayer player (\p -> p { playerHand = card : playerHand p })
            let newValue = handValue (playerHand player)
            if newValue > 21
                then do
                    showHand player
                    logAction $ playerName player ++ " busts!"
                    modifyPlayer player (\p -> p { playerMoney = playerMoney p - playerBet p })
                else if newValue == 21
                    then do
                        showHand player
                        logAction $ playerName player ++ " reaches 21!"
                    else playerAction player
        "S" -> do
            showHand player
            logAction $ playerName player ++ " stands."
        _ -> do
            logAction "Invalid action. Please choose H or S."
            playerAction player

dealerTurn :: Game ()
dealerTurn = do
    logAction "Dealer's turn"
    showDealerHand
    dealerAction

dealerAction :: Game ()
dealerAction = do
    dealer <- gets dealer
    let value = handValue (dealerHand dealer)
    if value < 17
        then do
            card <- dealCard
            modifyDealer (\d -> d { dealerHand = card : dealerHand d })
            showDealerHand
            dealerAction
        else do
            logAction $ "Dealer stands with " ++ show value
            when (value > 21) $ logAction "Dealer busts!"

showHand :: Player -> Game ()
showHand player = do
    let hand = playerHand player
    logAction $ playerName player ++ "'s hand: " ++ formatHand hand ++ " (Value: " ++ show (handValue hand) ++ ")"

formatHand :: Hand -> String
formatHand = unwords . map formatCard

formatCard :: Card -> String
formatCard (Card rank suit) = show rank ++ " of " ++ show suit

showDealerInitialHand :: Game ()
showDealerInitialHand = do
    dealer <- gets dealer
    let hand = dealerHand dealer
    logAction $ "Dealer's hand: " ++ formatCard (head hand) ++ ", [Hidden]"

showDealerHand :: Game ()
showDealerHand = do
    dealer <- gets dealer
    let hand = dealerHand dealer
    logAction $ "Dealer's hand: " ++ show hand ++ " (Value: " ++ show (handValue hand) ++ ")"

resolveBets :: Game ()
resolveBets = do
    players <- gets players
    dealer <- gets dealer
    let dealerScore = handValue (dealerHand dealer)
    mapM_ (resolvePlayerBet dealerScore) players

resolvePlayerBet :: Int -> Player -> Game ()
resolvePlayerBet dealerScore player = do
    let playerScore = handValue (playerHand player)
    if playerScore > 21
        then logAction $ playerName player ++ " loses bet"
        else if dealerScore > 21 || playerScore > dealerScore
            then do
                logAction $ playerName player ++ " wins bet"
                modifyPlayer player (\p -> p { playerMoney = playerMoney p + playerBet p })
            else if playerScore == dealerScore
                then do
                    logAction $ playerName player ++ " pushes"
                    modifyPlayer player (\p -> p { playerMoney = playerMoney p })
                else logAction $ playerName player ++ " loses"

playRound :: Game ()
playRound = do
    logAction "Starting new round"
    gs <- get
    shuffledDeck <- liftIO $ shuffleDeck fullDeck
    put gs { deck = shuffledDeck }
    
    getBets
    dealInitialCards
    mapM_ playerTurn (players gs)
    dealerTurn
    resolveBets
    
    logAction "Round finished"

getBets :: Game ()
getBets = do
    players <- gets players
    mapM_ getPlayerBet players

getPlayerBet :: Player -> Game ()
getPlayerBet player = do
    GameConfig {minBet, maxBet} <- ask
    bet <- liftIO $ do
        putStrLn $ playerName player ++ ", enter your bet (min: " ++ show minBet ++ ", max: " ++ show maxBet ++ "):"
        readMaybe <$> getLine
    case bet of
        Just b | b >= minBet && b <= maxBet && b <= playerMoney player -> 
            modifyPlayer player (\p -> p { playerBet = b })
        _ -> do
            logAction "Invalid bet. Please try again."
            getPlayerBet player

logAction :: String -> Game ()
logAction = tell . (:[])

runGame :: GameConfig -> GameState -> IO ()
runGame config initialState = do
    ((_, finalState), logs) <- runWriterT $ runReaderT (runStateT gameLoop initialState) config
    mapM_ putStrLn logs
    putStrLn "Final game state:"
    print finalState

gameLoop :: Game ()
gameLoop = do
    playRound
    continuePlaying <- liftIO $ do
        putStrLn "Do you want to play another round? (y/n)"
        answer <- getLine
        return $ answer == "y"
    when continuePlaying gameLoop

initializeGame :: IO (GameConfig, GameState)
initializeGame = do
    putStrLn "Enter number of players:"
    numPlayers <- readLn
    players <- forM [1..numPlayers] $ \i -> do
        putStrLn $ "Enter name for player " ++ show i ++ ":"
        name <- getLine
        return $ Player name [] 0 1000
    
    let config = GameConfig 1000 10 500
        initialState = GameState players (Dealer []) fullDeck
    
    return (config, initialState)

main :: IO ()
main = do
    putStrLn "Welcome to Blackjack!"
    (config, initialState) <- initializeGame
    runGame config initialState