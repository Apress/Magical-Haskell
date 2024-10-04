{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings #-}

module Mongo.SystemPrompts
(
    SystemPromptData(..),
    insertSPD,
    findAllSPD
)
where 
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), FromJSON, Value (Object))
import qualified Util.AesonBson as AB
import Database.MongoDB (Collection, insert, Value, find, Select (select), Cursor)
import Mongo.Core (withMongoConnection, MongoConnection)


data SystemPromptData = SystemPromptData {
    title::Text,
    sysPrompt :: Text,
    createdAt :: UTCTime
} deriving (Show, Generic)

instance ToJSON SystemPromptData
instance FromJSON SystemPromptData

spdColName :: Collection
spdColName = "systemPrompts"

makeSPD :: Text -> Text -> IO SystemPromptData
makeSPD t sp = getCurrentTime >>= \x -> pure (SystemPromptData t sp x)

insertSPD :: MongoConnection -> Text -> Text -> IO Database.MongoDB.Value
insertSPD conn t sp = do
    spd <- makeSPD t sp
    let (Object jsn) = toJSON spd
    let bsn = AB.bsonifyBound jsn
    withMongoConnection conn $ insert spdColName bsn

-- findAllSPD :: MongoConnection -> 
findAllSPD :: MongoConnection -> IO Cursor
findAllSPD conn = withMongoConnection conn $ find (select [] spdColName)

