{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module WebAPI.WebApp
where

import Middleware (Mid, lgDbg)
import Web.Scotty.Trans (ScottyT, scottyT, get, text, status, ActionT, middleware)
import Network.Wai (Response, Request (..))
import StackTypes (AppState, Settings)
import Network.Wai.Handler.Warp (Port)
import Init (initAll)
import Control.Monad.RWS (evalRWST, RWST (..))
import Network.HTTP.Types (status401)
import Data.ByteString.Char8 (unpack)
import Conduit (MonadUnliftIO (withRunInIO))

type WebApp = ScottyT Mid

instance MonadUnliftIO Mid where
    {-# INLINE withRunInIO #-}
    withRunInIO inner = RWST $ \r s -> 
        withRunInIO $ \runInIO -> do
            result <- inner $ \action -> do
                (a, s', w) <- runInIO (runRWST action r s)
                return a
            return (result, s, mempty)



tact :: Request -> Mid ()
tact req = do
    let path = rawPathInfo req
    lgDbg $ "Received request on " ++ unpack path ++ ":\n" ++ show (requestHeaders req)

loggingMiddleware sett st app req respond = do
    evalRWST (tact req) sett st
    app req respond


mainServer :: Port -> Settings -> AppState -> IO ()
mainServer port settings initSt = do
    scottyT port (convertResponse settings initSt) $ do
            middleware (loggingMiddleware settings initSt)
            get "/" $ text "Hello world in ScottyT"
            get "/error" $ do
                status status401
                text "Not authorized!"
            {-
            get "/api/node/list" $ do
                json defaultNodesResponse
            post "/api/appUserToken" $ do
                req <- request
                body <- liftIO $ requestBody req
                let sbody = decodeUtf8 (BL.fromStrict body)
                lift $ logDbg $ "Received appUserToken request:\n" ++ show sbody
                appUserToken
            post "/api/pipeline/:pipelineId/execute" $ do 
                apiExecutePipeline
            -}




convertResponse :: Settings -> AppState -> Mid Response -> IO Response
convertResponse sett st resp = do 
    (a, _) <- evalRWST resp sett st
    pure a