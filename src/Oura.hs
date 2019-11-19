{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Oura where

import           Control.Lens
import           Data.Aeson             (FromJSON (..), Options (..),
                                         ToJSON (..), defaultOptions,
                                         fieldLabelModifier, genericParseJSON,
                                         genericToEncoding)
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL
import           Data.Maybe             (maybeToList)
import qualified Data.Text              as T
import           Data.Time.Calendar     (Day)
import           Generics.Deriving.Base (Generic)
import           Network.Wreq           (FormParam (..), Options, Response,
                                         asJSON, customHistoriedMethodWith,
                                         defaults, getWith, header, hrRedirects,
                                         param, postWith, redirects,
                                         responseBody, responseHeader)

import           Oura.Types

baseURL :: String
baseURL = "https://cloud.ouraring.com"
authURL :: String
authURL = baseURL <> "/oauth/authorize"
tokenURL :: String
tokenURL = baseURL <> "/oauth/token"
redirectURI :: String
redirectURI = "https://httpbin.org/get"

apiBase :: String
apiBase = "https://api.ouraring.com/v1/"

userAgent :: BC.ByteString
userAgent = "github.com/dustin/oura 0.1"

defOpts :: Network.Wreq.Options
defOpts = defaults & header "User-Agent" .~ [userAgent]

data AuthInfo = AuthInfo {
  _clientID       :: String
  , _clientSecret :: String
  , _bearerToken  :: String
  } deriving(Show)

authInfo :: AuthInfo
authInfo = AuthInfo{_bearerToken="", _clientID="", _clientSecret=""}

fromToken :: String -> AuthInfo
fromToken t = authInfo{_bearerToken=t}

jsonOpts :: Data.Aeson.Options
jsonOpts = defaultOptions {
  fieldLabelModifier = dropWhile (== '_')
  }

data AuthResponse = AuthResponse {
  _access_token    :: String
  , _expires_in    :: Int
  , _refresh_token :: String
  } deriving(Generic, Show)

instance FromJSON AuthResponse where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON AuthResponse where
  toEncoding = genericToEncoding jsonOpts

authenticateURL :: AuthInfo -> IO (Maybe String)
authenticateURL AuthInfo{..} = do
  let opts = defOpts & redirects .~ 1
                     & param "response_type" .~ ["code"]
                     & param "redirect_uri" .~ [T.pack redirectURI]
                     & param "client_id" .~ [T.pack _clientID]
  r <- customHistoriedMethodWith "GET" opts authURL
  pure $ (baseURL <>) <$> r ^? hrRedirects . folded . _2 . responseHeader "Location" . to BC.unpack


authToken :: BL.ByteString -> AuthInfo -> IO AuthResponse
authToken tok AuthInfo{..} = do
  r <- asJSON =<< postWith defOpts tokenURL [
    "grant_type" := ("authorization_code" :: String),
    "code" := tok,
    "redirect_uri" := redirectURI,
    "client_id" := _clientID,
    "client_secret" := _clientSecret] :: IO (Response AuthResponse)
  pure $ r ^. responseBody

refreshAuth :: AuthInfo -> AuthResponse -> IO AuthResponse
refreshAuth AuthInfo{..} AuthResponse{..} = do
  r <- asJSON =<< postWith defOpts tokenURL [
    "grant_type" := ("refresh_token" :: String),
    "client_id" := _clientID,
    "client_secret" := _clientSecret,
    "refresh_token" := _refresh_token] :: IO (Response AuthResponse)
  pure $ r ^. responseBody

authOpts :: AuthInfo -> Network.Wreq.Options
authOpts AuthInfo{..} = defOpts & header "Authorization" .~ ["Bearer " <> BC.pack _bearerToken]

fetchAPI :: Monoid a => String -> Lens' OuraData (Maybe a) -> AuthInfo -> Maybe Day -> Maybe Day -> IO a
fetchAPI u l ai start end = do
  let opts = authOpts ai & param "start" .~ dp start
                         & param "end" .~ dp end
  r <- asJSON =<< getWith opts (apiBase <> u) :: IO (Response OuraData)
  pure $ r ^. responseBody . l . _Just

    where dp d = T.pack . show <$> maybeToList d

sleepPeriods :: AuthInfo -> Maybe Day -> Maybe Day -> IO [Sleep]
sleepPeriods = fetchAPI "sleep" sleep

activitySummaries :: AuthInfo -> Maybe Day -> Maybe Day -> IO [Activity]
activitySummaries = fetchAPI "activity" activity

readinessSummaries :: AuthInfo -> Maybe Day -> Maybe Day -> IO [Readiness]
readinessSummaries = fetchAPI "readiness" readiness

