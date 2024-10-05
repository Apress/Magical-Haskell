{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module VectorStorage.InMemory
where

import Data.Text
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Word (Word8)
import Mongo.MongoRAG (RAGData (vectorContent))

type MemoryStorage = V.Vector RAGData

dotProduct :: (Num a, U.Unbox a) => U.Vector a -> U.Vector a -> a
dotProduct v1 v2 = U.sum $ U.zipWith (*) v1 v2

dotProductAll :: U.Vector Float -> MemoryStorage -> V.Vector (Float, Int)
dotProductAll v = V.imap (\i el -> (v `dotProduct` vectorContent el, i))
