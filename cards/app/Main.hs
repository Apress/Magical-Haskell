{-#LANGUAGE  ScopedTypeVariables #-}
import           System.IO

import Cards
import Countable

main = ask >> getLine >>= toList >>= calcAverage >>= printResult
       where calcAverage l = pure $ (fromIntegral $ sum l) / (lengthF l)
             printResult n = putStrLn $ "Average of your list is: " ++ (show n)
             ask = putStrLn "Enter list of numbers, e.g. [1,3,4,5]:"
             lengthF = fromIntegral . length
             toList :: String -> IO [Int] = pure . read



-- main = print smallDeck >> putStrLn "Press Enter to deal the full deck" >> getLine >> mapM_ print fullDeck

safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x/y)