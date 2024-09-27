{-#LANGUAGE TypeSynonymInstances, ScopedTypeVariables, InstanceSigs #-}
module Cards
where

import Data.Functor
import Control.Applicative
import Data.Bifunctor
import Control.Monad
import Control.Monad.State.Lazy

-- simple algebraic data types for card values and suites
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Enum)
data CardSuite = Clubs | Spades | Diamonds | Hearts deriving (Eq, Enum)

-- our card type - merely combining CardValue and CardSuite
data Card = Card CardValue CardSuite deriving(Eq)

-- synonym for list of cards to store decks
type Deck = [Card]

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

smallDeck = [Card Ace Spades, Card Two Clubs, Card Jack Hearts]

data PlayerData a = PlayerData Deck a deriving (Show)

instance Functor PlayerData where
    fmap f (PlayerData d x) = PlayerData d (f x)

dealCards :: Int -> Deck -> (Deck, Deck)
dealCards n deck = let newDeck = drop n deck
                       plDeck  = take n deck
                   in (newDeck, plDeck)

countCards :: Deck -> (Int, Deck)
countCards deck = (length deck, deck)

newtype DeckM a = DeckM { runDeckM :: Deck -> (a, Deck)}


dealCardsM :: Int -> DeckM Deck
dealCardsM n = getDealerDeck >>= \deck -> 
                    let newDeck = drop n deck
                        plDeck  = take n deck
                    in putDealerDeck newDeck >> pure plDeck


countCardsM :: DeckM Int
countCardsM = getDealerDeck >>= pure . length

instance Functor DeckM where
    fmap :: (a -> b) -> DeckM a -> DeckM b
    fmap f (DeckM {runDeckM = rd}) = DeckM { runDeckM = \dd -> let (x, d) = rd dd in (f x, d) }


instance Applicative DeckM where
    pure x = DeckM { runDeckM = \d -> (x, d) }
    (<*>) (DeckM { runDeckM = rf} ) (DeckM {runDeckM = rx}) = DeckM {
                runDeckM = \d -> let (f, d')  = rf d
                                     (x, d'') = rx d'
                                 in  (f x, d'')
            }


instance Monad DeckM where
    (>>=) :: DeckM a -> (a -> DeckM b) -> DeckM b
    (>>=) (DeckM {runDeckM = rd}) f = DeckM { runDeckM = \d -> let (x, d') = rd d in runDeckM (f x) d' }

getDealerDeck :: DeckM Deck
getDealerDeck = DeckM {runDeckM = \d -> (d,d)}

putDealerDeck :: Deck -> DeckM ()
putDealerDeck deck = DeckM {runDeckM = \d -> ((),deck)}

type DeckM' = State Deck

dealCardsM' :: Int -> DeckM' Deck
dealCardsM' n = get >>= (pure . take n) >>= \plDeck -> (modify (drop n) >> pure plDeck)


countCardsM' :: DeckM' Int
countCardsM' = get >>= pure . length





