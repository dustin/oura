module Oura.Auth (loadAuth, loadAuthInfo, saveAuth) where

import           Data.Aeson (eitherDecodeFileStrict, encodeFile)
import           Oura

loadAuth :: FilePath -> IO AuthResponse
loadAuth path = either error id <$> (eitherDecodeFileStrict path :: IO (Either String AuthResponse))

saveAuth :: FilePath -> AuthResponse -> IO ()
saveAuth = encodeFile

loadAuthInfo :: FilePath -> String -> String -> IO AuthInfo
loadAuthInfo fp key secret =
  loadAuth fp >>= \AuthResponse{..} ->
                    pure AuthInfo{_bearerToken=_access_token, _clientID=key, _clientSecret=secret}
