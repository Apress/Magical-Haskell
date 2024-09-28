{-#LANGUAGE OverloadedStrings #-}

module IOFloor
where

import System.Random
import Data.List (sortOn)
import Text.Read (readMaybe)
import Cards

shuffleDeck :: Deck -> IO Deck
shuffleDeck deck = do
    gen <- newStdGen
    return $ map snd $ sortOn fst $ zip (randoms gen :: [Int]) deck

-- get a bet number from the player
getPlayerBet :: IO Int
getPlayerBet = do
    bet <- do
        putStrLn "Enter your bet (min: 10, max: 100):"
        readMaybe <$> getLine
    case bet of
        Just b | b >= 10 && b <= 100 -> 
            pure b
        _ -> do
            putStrLn "Invalid bet. Please try again."
            getPlayerBet