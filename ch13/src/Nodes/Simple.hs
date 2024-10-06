module Nodes.Simple
where
import Control.Arrow

testMonad :: IO ()
testMonad = do
    putStrLn "Hi, what's your name?"
    nm <- getLine
    putStrLn $ "Hi, " ++ nm

testArrow :: Kleisli IO (String, String) ()
testArrow = first (Kleisli putStrLn >>> Kleisli (const getLine)) 
    >>> arr (\(s1,s2) -> s2 ++ s1) >>> Kleisli putStrLn

runArrow = runKleisli testArrow


