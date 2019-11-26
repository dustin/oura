module Oura.Auth (loadAuth, loadAuthInfo, saveAuth) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (eitherDecodeFileStrict, encodeFile)
import           Oura

loadAuth :: MonadIO m => FilePath -> m AuthResponse
loadAuth path = either error id <$> (liftIO (eitherDecodeFileStrict path :: IO (Either String AuthResponse)))

saveAuth :: MonadIO m => FilePath -> AuthResponse -> m ()
saveAuth f r = liftIO (encodeFile f r)

loadAuthInfo :: MonadIO m => FilePath -> String -> String -> m AuthInfo
loadAuthInfo fp key secret =
  loadAuth fp >>= \AuthResponse{..} ->
                    pure AuthInfo{_bearerToken=_access_token, _clientID=key, _clientSecret=secret}
