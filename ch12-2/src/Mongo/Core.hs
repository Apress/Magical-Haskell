{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, NamedFieldPuns, FlexibleContexts #-}


module Mongo.Core
(
    MongoConnection,
    MongoState(..),
    initMongo,
    withMongoConnection,
    getPipe,
    getDatabase
)
where

import Database.MongoDB as MNG
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Configuration.Dotenv
import Configuration.Dotenv.Environment
import Network.Socket (PortNumber)
import Network.URI
import Data.List.Split (splitOn)
import Data.Maybe (maybe)
import Control.Monad.Except (throwError)
import qualified Util.AesonBson as AB

import Util.Logger





-- second var - database name!
data MongoConnection = MongoReplica ReplicaSet T.Text | MongoPipe Pipe T.Text

getPipe :: MongoConnection -> IO Pipe
getPipe (MongoReplica rs _) = primary rs
getPipe (MongoPipe p _) = return p

getDatabase :: MongoConnection -> IO T.Text
getDatabase (MongoReplica _ d) = pure d
getDatabase (MongoPipe _ d) = pure d

data MongoString = MongoString {
    hostName :: String,
    portNum :: PortNumber,
    dbUser :: String,
    dbPassword :: String,
    authDBName :: String,
    mainDBName :: String,
    isSrv :: Bool
} deriving (Show)

-- state for the mongodb
data MongoState = MongoState {
    mainConnection :: MongoConnection
}




-- needed for parseMongoStr
splitCredentials :: String -> (String, String)
splitCredentials input = 
    let parts = splitOn ":" input
        username = head parts
        passwordWithAt = last parts
        password = if not (null passwordWithAt) && last passwordWithAt == '@'
                   then init passwordWithAt
                   else passwordWithAt
    in (username, password)


{-
initMongo = do 
    k <- parseMongoStr "mongodb+srv://integrail:password@localhost:27017/nest?directConnection=true&authSource=admin"
    print k
-}

parseMongoStr str = do
    maybe (throwError $ userError $ "wrong mongo connection string! -- " ++ str)
          (\y -> do
                    let sch = uriScheme y
                    let dbn = tail $ uriPath y
                    let qr = uriQuery y
                    maybe (throwError $ userError $ "wrong mongo connection string! -- " ++ str)
                          (\ua -> do
                            let (usr, pwd) = splitCredentials $ uriUserInfo ua
                            let port = read (tail (uriPort ua)) :: PortNumber
                            let host = uriRegName ua
                            return $ MongoString {
                                        hostName = host,
                                        portNum = port,
                                        dbUser = usr,
                                        dbPassword = pwd,
                                        authDBName = "admin",
                                        mainDBName = dbn,
                                        isSrv = False
                                    })
                          (uriAuthority y))
                    
          (parseURI str) 
        


-- connectToMongo :: MongoString -> IO () --MongoConnection
-- Currently NO SUPPORT FOR SRV CASES!!!
connectToMongo MongoString {
    hostName,
    portNum,
    dbUser,
    dbPassword,
    authDBName,
    mainDBName,
    isSrv = False
} lgs = do
    lg_inf ("Connecting to " ++ hostName) lgs
    pipe <- connect $ Host hostName (PortNumber portNum)
    lg_suc "Connected" lgs
    is_auth <- access pipe master (T.pack authDBName) $ auth (T.pack dbUser) (T.pack dbPassword)
    if is_auth == False then lg_ftl "Mongo Authentication FAILED" lgs >> (throwError $ userError "Mongo Authentication Failed, FATAL")
    else do
        lg_suc "Mongo Authentication success" lgs
        return $ MongoPipe pipe (T.pack mainDBName)



-- mongo initialization function
initMongo :: LoggerState -> IO MongoState
initMongo lgs = do
    -- we know its there because we checked environment
    (Just muri1) <- lookupEnv "MAIN_MONGO_URI"
    ms1 <- parseMongoStr muri1
    bdb <- connectToMongo ms1 lgs
    return $ MongoState {
        mainConnection = bdb
    }


-- run a mongo action act using our connection conn
withMongoConnection :: MongoConnection -> Action IO a -> IO a
withMongoConnection conn act = do
    pipe <- getPipe conn
    db <- getDatabase conn
    MNG.access pipe master db act 


