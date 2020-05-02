{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oura.Influx (allLines, sleepLines, readinessLines, activityLines,
                   lastTimestamp) where

import           Control.Lens           ((^.), _Just)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Time.Calendar     (Day)
import           Data.Time.Clock        (UTCTime (..))
import           Data.Time.LocalTime    (ZonedTime, zonedTimeToUTC)
import qualified Data.Vector            as V
import           Database.InfluxDB

import           Oura.Types

ts :: ZonedTime -> Maybe UTCTime
ts = Just . zonedTimeToUTC

sleepLines :: Map Key Key -> Sleep -> [Line UTCTime]
sleepLines tags s@Sleep{..} = Line "sleep" tags (Map.fromList [
                                                    ("duration", i _sleep_duration),
                                                    ("total", i _sleep_total),
                                                    ("awake", i _sleep_awake),
                                                    ("rem", i _sleep_rem),
                                                    ("light", i _sleep_light),
                                                    ("deep", i _sleep_deep),
                                                    ("efficiency", i _sleep_efficiency),
                                                    ("onset_latency", i _sleep_onset_latency),
                                                    ("midpoint_time", i _sleep_midpoint_time),
                                                    -- ...
                                                    ("score", i _sleep_score)
                                                    ]) (ts _sleep_bedtime_start) : subs
  where
    i = FieldInt . toEnum
    subs = hyps <> hrs <> rmss

    storeInt n f (t,p) = Line n tags (Map.singleton "value" (i . f $ p)) (ts t)

    hyps = storeInt "hypnogram" fromEnum <$> hypnogram s
    hrs = storeInt "sleep_hr" id <$> sleepHR s
    rmss = storeInt "sleep_rmssd" id <$> sleepRMSSD s

dayTe :: Day -> UTCTime
dayTe d = UTCTime d 0

allLines :: Map Key Key -> OuraData -> [Line UTCTime]
allLines tags od = mconcat [
  foldMap (activityLines tags) (v activity),
  foldMap (readinessLines tags) (v readiness),
  foldMap (sleepLines tags) (v sleep)
  ]

  where v f = od ^. f . _Just

readinessLines :: Map Key Key -> Readiness -> [Line UTCTime]
readinessLines tags Readiness{..} =
  [Line "readiness" tags (Map.fromList [
                             ("score", i _readiness_score),
                             ("activity_balance", i _readiness_score_activity_balance),
                             ("previous_day", i _readiness_score_previous_day),
                             ("previous_night", i _readiness_score_previous_night),
                             ("recovery_index", i _readiness_score_recovery_index),
                             ("resting_hr", i _readiness_score_resting_hr),
                             ("sleep_balance", i _readiness_score_sleep_balance),
                             ("temperature", i _readiness_score_temperature)
                             ]) (Just . dayTe $ _readiness_summary_date)]
  where
    i = FieldInt . toEnum

activityLines :: Map Key Key -> Activity -> [Line UTCTime]
activityLines tags a@Activity{..} =
  Line "activity" tags (Map.fromList [
                           ("avg_met", d _activity_average_met),
                           ("daily_movement", i _activity_daily_movement),
                           ("high", i _activity_high),
                           ("medium", i _activity_medium),
                           ("low", i _activity_low),
                           ("rest", i _activity_rest),
                           ("steps", i _activity_steps),
                           ("target_km", d _activity_target_km),
                           ("to_target_km", d _activity_to_target_km),
                           ("total", i _activity_total)
                           ]) (ts _activity_day_start) : subs
  where
    subs = classes <> mins
    i = FieldInt . toEnum
    d = FieldFloat
    storeInt n f (t,p) = Line n tags (Map.singleton "value" (i . f $ p)) (ts t)
    storeDouble n f (t,p) = Line n tags (Map.singleton "value" (d . f $ p)) (ts t)

    classes = storeInt "act_classes" fromEnum <$> activityClasses a
    mins = storeDouble "act_met1mins" id <$> met1Mins a

newtype TSOnly = TSOnly UTCTime

instance QueryResults TSOnly where
  parseResults prec = parseResultsWithDecoder strictDecoder $ \_ _ columns fields ->
    TSOnly <$> (getField "time" columns fields >>= parseUTCTime prec)

lastTimestamp :: (MonadFail m, MonadIO m) => QueryParams -> m UTCTime
lastTimestamp p = do
  r <- liftIO (query p "select last(total)  from sleep" :: IO (V.Vector TSOnly))
  when (null r) $ fail "no results returned"
  let (TSOnly x) = V.head r
  pure x
