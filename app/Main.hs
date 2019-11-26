{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (ReaderT (..), asks, runReaderT)
import           Data.Aeson             (eitherDecode)
import qualified Data.ByteString.Lazy   as BL
import           Data.List.Extra        (chunksOf)
import qualified Data.Map.Strict        as Map
import           Data.String            (fromString)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime (..))
import           Database.InfluxDB      (Key, Line (..), Precision (Second),
                                         host, precision, queryParams, server,
                                         writeBatch, writeParams)
import           Database.InfluxDB.Line (tagSet)
import           Options.Applicative    (Parser, execParser, fullDesc, help,
                                         helper, info, long, progDesc,
                                         showDefault, strOption, value, (<**>))

import           Oura
import           Oura.Auth
import           Oura.Influx
import           Oura.Types

newtype Env = Env { opts :: Options }

type Processor = ReaderT Env IO

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

storeLines :: [Line UTCTime] -> Processor ()
storeLines ls = do
  Options{optInfluxDB,optInfluxDBHost} <- asks opts
  u <- asks (optUser . opts)
  let wp = writeParams (fromString optInfluxDB) & server.host .~ optInfluxDBHost & precision .~ Second
      ls' = ls & traversed . tagSet %~ Map.insert "user" u
  liftIO $ mapM_ (writeBatch wp) $ chunksOf 2000 ls'

processFile :: Processor ()
processFile = do
  Options{..} <- asks opts
  ouraFile <- liftIO $ BL.readFile optDumpPath
  let oura = eitherDecode ouraFile :: Either String OuraData
      ls = either fail (allLines mempty) oura

  storeLines ls

processLive :: Processor ()
processLive = do
  Options{..} <- asks opts
  let qp = queryParams (fromString optInfluxDB) & server.host .~ optInfluxDBHost
  ai <- loadAuthInfo optAuthPath optAccess optSecret
  ts <- utctDay <$> lastTimestamp qp
  (liftIO.putStrLn) ("Fetching since: " <> show ts)
  oura <- fetchAll ai (Just ts) Nothing
  (liftIO.print) $ oura ^.. sleep . _Just . folded . sleep_summary_date
  storeLines (allLines mempty oura)

main :: IO ()
main = runReaderT run =<< Env <$> execParser o
  where o = info (options <**> helper) (fullDesc <> progDesc "Influx oura")
        run = do -- processFile <|> processLive
          f <- asks (optDumpPath . opts)
          if f == "" then processLive else processFile
