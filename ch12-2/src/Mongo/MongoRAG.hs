{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings #-}

module Mongo.MongoRAG
(
    RAGData(..),
    insertRAG,
    findAllRAG
)
where
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), FromJSON, Value (Object), fromJSON, Result (Success))
import qualified Util.AesonBson as AB
import Database.MongoDB (Collection, insert, Value, find, Select (select), Cursor, rest)
import Mongo.Core (withMongoConnection, MongoConnection, MongoState (mainConnection))

import qualified Data.Vector.Unboxed as U

data RAGData = RAGData {
    title::Maybe Text,
    textContent :: Text,
    vectorContent :: U.Vector Float
} deriving (Show, Generic)

instance ToJSON RAGData
instance FromJSON RAGData

ragColName :: Collection
ragColName = "vectorRAG"

-- LOWLEVEL interface
insertRAG :: MongoConnection -> RAGData -> IO Database.MongoDB.Value
insertRAG conn rgd = do
    let (Object jsn) = toJSON rgd
    let bsn = AB.bsonifyBound jsn
    withMongoConnection conn $ insert ragColName bsn

-- findAllSPD :: MongoConnection -> 
findAllRAG :: MongoConnection -> IO [RAGData]
findAllRAG conn = withMongoConnection conn $ do
    cur <- find (select [] ragColName)
    bsn <- rest cur
    let results = map (fromJSON . Object . AB.aesonify) bsn
    let res = [rag | Success rag <- results]  -- Extract successful conversions
    pure res
    

