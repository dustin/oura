module Oura.Auth (loadAuth, saveAuth) where

import           Data.Aeson (eitherDecodeFileStrict, encodeFile)
import           Oura

loadAuth :: FilePath -> IO AuthResponse
loadAuth path = either error id <$> (eitherDecodeFileStrict path :: IO (Either String AuthResponse))

saveAuth :: FilePath -> AuthResponse -> IO ()
saveAuth = encodeFile
