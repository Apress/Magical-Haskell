{-#LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}

module Countable
where

import Data.Functor
import Control.Applicative
import Data.Bifunctor
import Control.Monad

data Countable a = Countable Int a deriving (Show)

instance Eq a => Eq (Countable a) where
    (==) (Countable _ x) (Countable _ y) = x == y

instance Functor Countable where
    fmap f (Countable c x) = Countable (c+1) (f x)
    (<$) x (Countable c y) = Countable c x -- redefining to make Applicative calculations correct

instance Applicative Countable where
    pure x = Countable 0 x
    liftA2 func (Countable c1 x) (Countable c2 y) = Countable (c1 + c2) (func x y)
    (<*>) (Countable c1 func) (Countable c2 x) = Countable (c1 + c2) (func x)

instance Monad Countable where
    (>>=) (Countable c1 x) f = let (Countable c2 y) = f x
                               in Countable (c1 + c2) y

printCosts st costs curr = "Your costs are " ++ show ((length st) * costs) ++ " " ++ curr

stringComp = Countable 3 "Hello World"
costComp = Countable 5 10

printCostsA = printCosts <$> stringComp <*> costComp <*> (pure "USD")

veryCostlyFunction :: String -> Countable Int
veryCostlyFunction s = Countable 100 (length s)


-- pc = length <$> (pure "Hello World" :: Countable String) <*> (*10)





