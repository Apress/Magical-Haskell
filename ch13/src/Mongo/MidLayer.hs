{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Mongo.MidLayer
where

import Data.Kind (Type)
import Database.MongoDB (Value)
import Mongo.MongoRAG (RAGData, ragColName, insertRAG, findAllRAG)
import Data.Text (Text)
import MidMonad (Mid)
import Mongo.Core (MongoState(..))
import Control.Monad.MRWS
import StackTypes (Settings(..))

class MongoCollection a where
    type Record a :: Type
    colName :: Text
    insertOne :: Record a -> Mid Value
    findAll :: Mid [Record a]

instance MongoCollection RAGData where
    type Record RAGData = RAGData
    colName = ragColName
    insertOne rgo = do 
        conn <- mainConnection <$> asks mongoSettings
        liftIO $ insertRAG conn rgo
    findAll = asks mongoSettings >>= (liftIO . findAllRAG) . mainConnection


