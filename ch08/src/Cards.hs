{-#LANGUAGE OverloadedStrings #-}
module Cards
where

-- simple algebraic data types for card values and suites
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Enum)
data CardSuite = Clubs | Spades | Diamonds | Hearts deriving (Eq, Enum)

-- synonym for list of cards to store decks
type Deck = [Card]

-- our card type - merely combining CardValue and CardSuite
data Card = Card CardValue CardSuite deriving(Eq)

type Hand = [Card]

data Player = Player { 
    playerHand :: Hand
  , playerBet :: Int
  , playerMoney :: Int } deriving Show
data Dealer = Dealer { dealerHand :: Hand } deriving Show

data GameState = GameState
    { player :: Player
    , dealer :: Dealer
    , deck :: Deck
    } deriving Show

makeBet :: Int -> Player -> Player
makeBet bet pl = pl { playerBet = bet, playerMoney = (playerMoney pl) - bet}

payout :: Int -> Player -> Player
payout amount pl = pl { playerBet = 0, playerMoney = (playerMoney pl) + amount, playerHand = [] }

rank :: Card -> CardValue
rank (Card r _) = r

suit :: Card -> CardSuite
suit (Card _ s) = s

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
       then sum $ map (\c -> if rank c == Ace then 1 else cardValue c) hand
       else baseValue

instance Show CardValue where
  show c = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"] !! (fromEnum c)

instance Show CardSuite where
  show Spades   = "♠"
  show Clubs    = "♣"
  show Diamonds = "♦"
  show Hearts   = "♥"

-- defining show function that is a little nicer then default
instance Show Card where
  show (Card a b) = show a ++ show b

-- defining full deck of cards via comprehension; how cool is that?! :)
fullDeck :: Deck
fullDeck = [ Card x y | y <- [Clubs .. Hearts], x <- [Two .. Ace] ]

smallDeck :: Deck
smallDeck = [Card Ace Spades, Card Two Clubs, Card Jack Hearts]

dealCards :: Int -> Deck -> (Deck, Deck)
dealCards n deck = let newDeck = drop n deck
                       plDeck  = take n deck
                   in (newDeck, plDeck)

countCards :: Deck -> (Int, Deck)
countCards deck = (length deck, deck)

