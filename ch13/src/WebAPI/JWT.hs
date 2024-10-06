{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module WebAPI.JWT
where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, Object, genericToJSON, defaultOptions, genericParseJSON, toJSON, Value(..))
import qualified Data.Aeson as Aeson
import Data.Text (Text, unpack)
import Network.Wai (Request, requestMethod, rawPathInfo, rawQueryString, requestHeaders, requestBody, Middleware, responseLBS)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)

--import Web.Scotty
import Web.Scotty.Trans
import qualified Web.JWT as JWT


import Network.HTTP.Types.Status
import MidMonad (Mid)
import Control.Monad.MRWS (lift)
import Middleware (lgDbg)
import Web.JWT (decodeAndVerifySignature, hmacSecret)


-- change this in production!!!
mainSecretKey :: Text = "jarvis-new-generation-secret-token"

-- Define the Node data type
data UserToken = UserToken
  { user        :: Text
  , token       :: Text
  } deriving (Show, Generic)

-- Define the main data type
data TokenResponse = TokenResponse
  { status :: Text
  , result  :: UserToken
  } deriving (Show, Generic)

-- Derive ToJSON and FromJSON instances
instance ToJSON UserToken
instance FromJSON UserToken

instance ToJSON TokenResponse
instance FromJSON TokenResponse

appUserToken :: ActionT Mid ()
appUserToken = do 
            req <- request
            let maybeToken = extractBearerToken req
            case maybeToken of
                Nothing    -> liftIO $ putStrLn $ "No valid Bearer token found"
                Just token -> do
                    -- lift $ logDbg $ "Token: " <> (Data.Text.unpack token)
                    let dtk = JWT.decodeAndVerifySignature (JWT.toVerify . JWT.hmacSecret $ mainSecretKey) token
                    case dtk of
                        Nothing -> liftIO $ putStrLn $ "Illegal token!"
                        Just goodJWT -> do
                            let cl = JWT.unClaimsMap $ JWT.unregisteredClaims $ JWT.claims goodJWT
                            -- lift $ logDbg $ show goodJWT
                            lift $ lgDbg $ "Claims: \n" ++ show cl
                            -- ok creating a user token to return
                            let cs = mempty { -- mempty returns a default JWTClaimsSet
                                  JWT.iss = JWT.stringOrURI . T.pack $ "jarvis-haskell"
                                , JWT.unregisteredClaims = JWT.ClaimsMap $ Map.union (Map.fromList 
                                    [("user", String "email")]) cl
                                }
                            let key = JWT.hmacSecret mainSecretKey
                            let newToken = JWT.encodeSigned key mempty cs
                            -- lift $ logDbg $ show newToken
                            lift $ lgDbg "Returning new token"
                            json $ TokenResponse {
                                status = "ok",
                                result = UserToken
                                    { user = "user"
                                    , token = newToken
                                    }
                            }

-- Function to extract the bearer token from the Authorization header
extractBearerToken :: Request -> Maybe Text
extractBearerToken req = do
    authHeader <- lookup "Authorization" (requestHeaders req)
    let token = decodeUtf8 (BL.fromStrict authHeader)
    if "Bearer " `TL.isPrefixOf` token
        then Just (TL.toStrict (TL.drop 7 token)) -- Drop "Bearer " prefix and convert to strict Text
        else Nothing



jwtAuthMiddleware :: Middleware
jwtAuthMiddleware app req sendResponse = do
    let maybeToken = extractBearerToken req
    case maybeToken of
        Nothing -> sendResponse $ responseLBS status401 [] "Unauthorized"
        Just token -> do
            let valid = decodeAndVerifySignature (JWT.toVerify . JWT.hmacSecret $ mainSecretKey) token
            case valid of
                Nothing -> sendResponse $ responseLBS status401 [] "Invalid Token"
                Just _  -> app req sendResponse  -- Continue to the next middleware or main app
