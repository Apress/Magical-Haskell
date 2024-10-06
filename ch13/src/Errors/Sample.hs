
module Errors.Sample
where

import Control.Exception

testE1 :: IO ()
testE1 = do
    result <- try readNumbersFromFile :: IO (Either IOException [Int])
    case result of
        Left ex -> putStrLn $ "An error occurred: " ++ show ex
        Right numbers -> putStrLn $ "Read numbers: " ++ show numbers

readNumbersFromFile :: IO [Int]
readNumbersFromFile = do
    content <- readFile "numbers"
    return (map read (lines content))

testE2 :: IO ()
testE2 = do
    catch (printDivisor 10 0) handler

printDivisor :: Integer -> Integer -> IO ()
printDivisor x y = do
    putStrLn $ "The divisor of " ++ show x ++ " by " ++ show y ++ " is " ++ show (x `div` y)

handler :: ArithException -> IO ()
handler ex = putStrLn $ "Caught an arithmetic exception: " ++ show ex

testE3 :: IO ()
testE3 = handle handler $ printDivisor 10 0




