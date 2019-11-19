{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (isAlphaNum)
import           Options.Applicative  (Parser, execParser, fullDesc, help,
                                       helper, info, long, progDesc, short,
                                       showDefault, strOption, switch, value,
                                       (<**>))
import           Oura
import           Oura.Auth

data Options = Options {
  optPath      :: FilePath
  , optAccess  :: String
  , optSecret  :: String
  , optRefresh :: Bool
  }

options :: Parser Options
options = Options
  <$> strOption (long "authpath" <> showDefault <> value "auth.json" <> help "auth storage path")
  <*> strOption (long "access_key" <> value "" <> help "access key")
  <*> strOption (long "access_secret" <> value "" <> help "access secret")
  <*> switch (long "refresh" <> short 'r' <> help "refresh instead of login")

login :: Options -> IO ()
login Options{..} = do
  let ai = AuthInfo{_bearerToken="", _clientID=optAccess, _clientSecret=optSecret}
  Just url <- authenticateURL ai
  putStrLn ("go to this URL and give me the code: " <> url)
  code <- BL.takeWhile (isAlphaNum . toEnum . fromEnum) . BL.fromStrict <$> BS.getLine
  ar <- authToken code ai
  saveAuth optPath ar

refresh :: Options -> IO ()
refresh Options{..} =
  loadAuth optPath >>= refreshAuth authInfo{_clientID=optAccess, _clientSecret=optSecret} >>= saveAuth optPath

run :: Options -> IO ()
run opts@Options{..}
  | optRefresh = refresh opts
  | otherwise  = login opts

main :: IO ()
main = run =<< execParser opts

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Authenticate to Oura")
