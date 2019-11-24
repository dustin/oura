{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Lens
import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import           Data.List.Extra      (chunksOf)
import qualified Data.Map.Strict      as Map
import           Data.String          (fromString)
import           Data.Text            (Text)
import           Data.Time.Clock      (UTCTime (..))
import           Database.InfluxDB    (Key, Line (..), Precision (Second),
                                       QueryParams, WriteParams, host,
                                       precision, queryParams, server,
                                       writeBatch, writeParams)
import           Options.Applicative  (Parser, execParser, fullDesc, help,
                                       helper, info, long, progDesc,
                                       showDefault, strOption, value, (<**>))

import           Oura
import           Oura.Auth
import           Oura.Influx
import           Oura.Types

data Options = Options {
  optInfluxDBHost :: Text
  , optInfluxDB   :: String
  , optDumpPath   :: FilePath
  , optAuthPath   :: FilePath
  , optAccess     :: String
  , optSecret     :: String
  , optUser       :: Key
  }

options :: Parser Options
options = Options
  <$> strOption (long "dbhost" <> showDefault <> value "localhost" <> help "influxdb host")
  <*> strOption (long "dbname" <> showDefault <> value "influxer" <> help "influxdb database")
  <*> strOption (long "ouradump" <> showDefault <> value "" <> help "path to dumped data from oura")
  <*> strOption (long "authpath" <> showDefault <> value "auth.json" <> help "auth storage path")
  <*> strOption (long "access_key" <> value "" <> help "access key")
  <*> strOption (long "access_secret" <> value "" <> help "access secret")
  <*> strOption (long "user" <> value "" <> help "user tag")

storeLines :: WriteParams -> [Line UTCTime] -> IO ()
storeLines wp ls = mapM_ (writeBatch wp) $ chunksOf 1000 ls

qParams :: Options -> QueryParams
qParams Options{..} = queryParams (fromString optInfluxDB) & server.host .~ optInfluxDBHost

wParams :: Options -> WriteParams
wParams Options{..} = writeParams (fromString optInfluxDB) & server.host .~ optInfluxDBHost & precision .~ Second

processFile :: Options -> IO ()
processFile opts@Options{..} = do
  ouraFile <- BL.readFile optDumpPath
  let oura = eitherDecode ouraFile :: Either String OuraData
      tags = Map.singleton "user" optUser
      ls = either fail (allLines tags) oura

  storeLines (wParams opts) ls

processLive :: Options -> IO  ()
processLive opts@Options{..} = do
  ai <- loadAuthInfo optAuthPath optAccess optSecret
  ts <- succ . utctDay <$> lastTimestamp (qParams opts)
  putStrLn ("Fetching since: " <> show ts)
  oura <- fetchAll ai (Just ts) Nothing
  let tags = Map.singleton "user" optUser
  storeLines (wParams opts) (allLines tags oura)

run :: Options -> IO ()
run opts@Options{optDumpPath=""} = processLive opts
run opts                         = processFile opts

main :: IO ()
main = run =<< execParser opts
  where opts = info (options <**> helper) (fullDesc <> progDesc "Influx oura")
