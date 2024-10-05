{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module VectorStorage.InMemory
(sortedSearchResults,
MemoryStorage,
addRAGData)
where

import Data.Text
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Ord (comparing, Down(..))
import Mongo.MongoRAG (RAGData (vectorContent, textContent))
import Data.Vector.Algorithms.Intro (sortBy)
import Control.Monad.ST (runST)

type MemoryStorage = V.Vector RAGData

-- Function to add a new RAGData item to the MemoryStorage
addRAGData :: MemoryStorage -> RAGData -> MemoryStorage
addRAGData = V.snoc

sortedSearchResults :: Int -> U.Vector Float -> MemoryStorage -> V.Vector (Text, Float)
sortedSearchResults n v ms = 
    let sorted = sortedSearch v ms
    in  V.map (\(score, ind) -> (textContent (ms V.! ind), score)) (V.take n sorted)
                            

sortedSearch :: U.Vector Float -> MemoryStorage -> V.Vector (Float, Int)
sortedSearch v ms = let x = dotProductAll v ms
                    in  sortVectorByFloat x

dotProduct :: (Num a, U.Unbox a) => U.Vector a -> U.Vector a -> a
dotProduct v1 v2 = U.sum $ U.zipWith (*) v1 v2

dotProductAll :: U.Vector Float -> MemoryStorage -> V.Vector (Float, Int)
dotProductAll v = V.imap (\i el -> (v `dotProduct` vectorContent el, i))

-- Function to sort a boxed vector of tuples (Float, Int) by the Float component
sortVectorByFloat :: V.Vector (Float, Int) -> V.Vector (Float, Int)
sortVectorByFloat = V.modify (sortBy (comparing (Down . fst)))

-- Function to sort a boxed vector of tuples (Float, Int) by the Float component
sortVectorByFloatM :: V.Vector (Float, Int) -> V.Vector (Float, Int)
sortVectorByFloatM vec = runST $ do
    -- Create a mutable copy of the vector
    mvec <- V.thaw vec
    -- Sort the mutable vector in-place
    sortBy (comparing (Down . fst)) mvec
    -- Freeze the mutable vector back to an immutable one
    V.unsafeFreeze mvec
