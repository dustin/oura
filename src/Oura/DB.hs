{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oura.DB where

import           Data.Time.LocalTime              (ZonedTime, zonedTimeToUTC)
import           Database.SQLite.Simple           hiding (bind, close)
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToField

import           Oura.Types

instance ToField ZonedTime where
  toField = toField . zonedTimeToUTC

instance ToField SleepPhase where
  toField = toField . fromEnum

instance FromField SleepPhase where
  fromField f = case fieldData f of
                  (SQLInteger x) -> Ok . toEnum . fromEnum $ x
                  _ -> returnError ConversionFailed f "invalid type for SleepPhase"

dbInit :: Connection -> IO ()
dbInit db = do
  execute_ db "create table if not exists hypnogram (ts timestamp, phase int)"
  execute_ db "create unique index if not exists hypno_ts on hypnogram(ts)"
  execute_ db $ mconcat [
    "create table if not exists sleep (awake int, ",
    "                                  bedtime_end           timestamp, ",
    "                                  bedtime_end_delta     int, ",
    "                                  bedtime_start         timestamp, ",
    "                                  bedtime_start_delta   int, ",
    "                                  breath_average        real, ",
    "                                  deep                  int, ",
    "                                  duration              int, ",
    "                                  efficiency            int, ",
    "                                  hr_average            real, ",
    "                                  hr_lowest             int, ",
    "                                  is_longest            int, ",
    "                                  light                 int, ",
    "                                  midpoint_at_delta     int, ",
    "                                  midpoint_time         int, ",
    "                                  onset_latency         int, ",
    "                                  period_id             int, ",
    "                                  rem                   int, ",
    "                                  restless              int, ",
    "                                  rmssd                 int, ",
    "                                  score                 int, ",
    "                                  score_alignment       int, ",
    "                                  score_deep            int, ",
    "                                  score_disturbances    int, ",
    "                                  score_efficiency      int, ",
    "                                  score_latency         int, ",
    "                                  score_rem             int, ",
    "                                  score_total           int, ",
    "                                  summary_date          date, ",
    "                                  temperature_delta     real, ",
    "                                  temperature_deviation real, ",
    "                                  timezone              int, ",
    "                                  total                 int)"]
  execute_ db "create unique index if not exists sleep_ts on sleep(bedtime_start)"
  execute_ db "create table if not exists sleep_hr (ts timestamp, hr int)"
  execute_ db "create unique index if not exists sleep_hr_ts on sleep_hr(ts)"
  execute_ db "create table if not exists sleep_rmssd (ts timestamp, hr int)"
  execute_ db "create unique index if not exists sleep_rmssd_ts on sleep_rmssd(ts)"

instance ToRow Sleep where
  toRow Sleep{..} = [
    toField _sleep_awake,
    toField _sleep_bedtime_end,
    toField _sleep_bedtime_end_delta,
    toField _sleep_bedtime_start,
    toField _sleep_bedtime_start_delta,
    toField _sleep_breath_average,
    toField _sleep_deep,
    toField _sleep_duration,
    toField _sleep_efficiency,
    toField _sleep_hr_average,
    toField _sleep_hr_lowest,
    toField _sleep_is_longest,
    toField _sleep_light,
    toField _sleep_midpoint_at_delta,
    toField _sleep_midpoint_time,
    toField _sleep_onset_latency,
    toField _sleep_period_id,
    toField _sleep_rem,
    toField _sleep_restless,
    toField _sleep_rmssd,
    toField _sleep_score,
    toField _sleep_score_alignment,
    toField _sleep_score_deep,
    toField _sleep_score_disturbances,
    toField _sleep_score_efficiency,
    toField _sleep_score_latency,
    toField _sleep_score_rem,
    toField _sleep_score_total,
    toField _sleep_summary_date,
    toField _sleep_temperature_delta,
    toField _sleep_temperature_deviation,
    toField _sleep_timezone,
    toField _sleep_total
    ]

-- λ> ouraFile <- BL.readFile "/Users/dsal/Downloads/oura_2019-11-17T05-14-10.json"
-- λ> oura = eitherDecode ouraFile :: Either String OuraData
-- λ> saveSleeps db (oura ^. folded . sleep)

saveSleeps :: Connection -> [Sleep] -> IO ()
saveSleeps db sleeps = do
  executeMany db "insert into sleep values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)" sleeps
  executeMany db "insert into hypnogram values(?, ?)" $ foldMap hypnogram sleeps
  executeMany db "insert into sleep_hr values(?, ?)" $ foldMap sleepHR sleeps
  executeMany db "insert into sleep_rmssd values(?, ?)" $ foldMap sleepRMSSD sleeps
