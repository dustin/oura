{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Oura.Types where

import           Control.Lens
import           Data.Aeson
import           Data.Maybe          (fromMaybe)
import           Data.Time.Calendar  (Day)
import           Data.Time.Clock     (NominalDiffTime, addUTCTime)
import           Data.Time.LocalTime (ZonedTime (..), utcToZonedTime,
                                      zonedTimeToUTC)
import           GHC.Generics        (Generic)

data ActivityClass = NonWear
                   | Rest
                   | Inactive
                   | LowIntensity
                   | MediumIntensity
                   | HighIntensity
                   deriving (Show, Enum, Bounded)

charToClass :: Char -> ActivityClass
charToClass '0' = NonWear
charToClass '1' = Rest
charToClass '2' = Inactive
charToClass '3' = LowIntensity
charToClass '4' = MediumIntensity
charToClass '5' = HighIntensity
charToClass _   = error "invalid activity class"

-- https://cloud.ouraring.com/docs/activity
data Activity = Activity {
  _activity_average_met                :: Double
  , _activity_cal_active               :: Int
  , _activity_cal_total                :: Int
  , _activity_class_5min               :: String
  , _activity_daily_movement           :: Int
  , _activity_day_end                  :: ZonedTime
  , _activity_day_start                :: ZonedTime
  , _activity_high                     :: Int
  , _activity_inactive                 :: Int
  , _activity_inactivity_alerts        :: Int
  , _activity_low                      :: Int
  , _activity_medium                   :: Int
  , _activity_met_1min                 :: [Double]
  , _activity_met_min_high             :: Double
  , _activity_met_min_inactive         :: Double
  , _activity_met_min_low              :: Double
  , _activity_met_min_medium           :: Double
  , _activity_non_wear                 :: Double
  , _activity_rest                     :: Int
  , _activity_score                    :: Int
  , _activity_score_meet_daily_targets :: Int
  , _activity_score_move_every_hour    :: Int
  , _activity_score_recovery_time      :: Int
  , _activity_score_stay_active        :: Int
  , _activity_score_training_frequency :: Int
  , _activity_score_training_volume    :: Int
  , _activity_steps                    :: Int
  , _activity_summary_date             :: Day
  , _activity_target_calories          :: Int
  , _activity_target_km                :: Double
  , _activity_target_miles             :: Double
  , _activity_timezone                 :: Int
  , _activity_to_target_km             :: Double
  , _activity_to_target_miles          :: Double
  , _activity_total                    :: Int
  } deriving (Generic, Show)

-- | Activity classes per 5 minutes from the given Activity.
activityClasses :: Activity -> [(ZonedTime, ActivityClass)]
activityClasses Activity{_activity_day_start, _activity_class_5min} =
  zip (windows _activity_day_start 300) (charToClass <$> _activity_class_5min)

-- | Average MET level for each minute of the activity period, starting from 4 AM local time.
met1Mins :: Activity -> [(ZonedTime, Double)]
met1Mins Activity{_activity_day_start, _activity_met_1min} =
  zip (windows _activity_day_start 60) _activity_met_1min

-- Compute time windows at the given interval from the start of an Activity
windows :: ZonedTime -> NominalDiffTime -> [ZonedTime]
windows start d = [addSecs x start | x <- [0,d..]]
  where
    addSecs :: NominalDiffTime -> ZonedTime -> ZonedTime
    addSecs x t@ZonedTime{zonedTimeZone} = utcToZonedTime zonedTimeZone (x `addUTCTime` zonedTimeToUTC t)

withoutPrefix :: String -> Data.Aeson.Options
withoutPrefix p = defaultOptions{fieldLabelModifier=drop (length p)}

instance FromJSON Activity where
  parseJSON = genericParseJSON (withoutPrefix "_activity_")

makeLenses ''Activity

-- https://cloud.ouraring.com/docs/readiness
data Readiness = Readiness {
  _readiness_period_id                :: Int
  , _readiness_score                  :: Int
  , _readiness_score_activity_balance :: Int
  , _readiness_score_previous_day     :: Int
  , _readiness_score_previous_night   :: Int
  , _readiness_score_recovery_index   :: Int
  , _readiness_score_resting_hr       :: Int
  , _readiness_score_sleep_balance    :: Int
  , _readiness_score_temperature      :: Int
  , _readiness_summary_date           :: Day
  } deriving (Generic, Show)

instance FromJSON Readiness where
  parseJSON = genericParseJSON (withoutPrefix "_readiness_")

makeLenses ''Readiness

-- Not documented well.
data RestfulPeriod = RestfulPeriod {
  _restful_period_bedtime_end      :: ZonedTime
  , _restful_period_bedtime_start  :: ZonedTime
  , _restful_period_breath_average :: Double
  , _restful_period_duration       :: Int
  , _restful_period_hr_average     :: Maybe Double
  , _restful_period_period_id      :: Int
  , _restful_period_summary_date   :: Day
  , _restful_period_timezone       :: Int
  } deriving (Generic, Show)

instance FromJSON RestfulPeriod where
  parseJSON = genericParseJSON (withoutPrefix "_restful_period_")

makeLenses ''RestfulPeriod

-- https://cloud.ouraring.com/docs/sleep
data Sleep = Sleep {
  _sleep_awake                   :: Int
  , _sleep_bedtime_end           :: ZonedTime
  , _sleep_bedtime_end_delta     :: Int
  , _sleep_bedtime_start         :: ZonedTime
  , _sleep_bedtime_start_delta   :: Int
  , _sleep_breath_average        :: Double
  , _sleep_deep                  :: Int
  , _sleep_duration              :: Int
  , _sleep_efficiency            :: Int
  , _sleep_hr_5min               :: Maybe [Int]
  , _sleep_hr_average            :: Maybe Double
  , _sleep_hr_lowest             :: Maybe Int
  , _sleep_hypnogram_5min        :: String
  , _sleep_is_longest            :: Int
  , _sleep_light                 :: Int
  , _sleep_midpoint_at_delta     :: Int
  , _sleep_midpoint_time         :: Int
  , _sleep_onset_latency         :: Int
  , _sleep_period_id             :: Int
  , _sleep_rem                   :: Int
  , _sleep_restless              :: Int
  , _sleep_rmssd                 :: Maybe Int
  , _sleep_rmssd_5min            :: Maybe [Int]
  , _sleep_score                 :: Int
  , _sleep_score_alignment       :: Int
  , _sleep_score_deep            :: Int
  , _sleep_score_disturbances    :: Int
  , _sleep_score_efficiency      :: Int
  , _sleep_score_latency         :: Int
  , _sleep_score_rem             :: Int
  , _sleep_score_total           :: Int
  , _sleep_summary_date          :: Day
  , _sleep_temperature_delta     :: Maybe Double
  , _sleep_temperature_deviation :: Maybe Double
  , _sleep_timezone              :: Int
  , _sleep_total                 :: Int
  } deriving (Generic, Show)

-- Average heart rate for each beginning 5 minutes of the sleep
-- period, the first period starting from sleep.bedtime_start.
sleepHR :: Sleep -> [(ZonedTime, Int)]
sleepHR Sleep{_sleep_bedtime_start, _sleep_hr_5min} =
  zip (windows _sleep_bedtime_start 300) (fromMaybe [] _sleep_hr_5min)

-- The average HRV (calculated using rMSSD method) for each beginning
-- 5 minutes of the sleep period, the first period starting from
-- sleep.bedtime_start.
sleepRMSSD :: Sleep -> [(ZonedTime, Int)]
sleepRMSSD Sleep{_sleep_bedtime_start, _sleep_rmssd_5min} =
  zip (windows _sleep_bedtime_start 300) (fromMaybe [] _sleep_rmssd_5min)

data SleepPhase = Deep | Light | REM | Awake deriving(Enum, Show)

charToPhase :: Char -> SleepPhase
charToPhase '1' = Deep
charToPhase '2' = Light
charToPhase '3' = REM
charToPhase '4' = Awake
charToPhase _   = error "invalid sleep phase in hypnogram"

-- A string that contains one character for each starting five minutes
-- of the sleep period, so that the first period starts from
-- sleep.bedtime.start: - '1' = deep (N3) sleep - '2' = light (N1 or
-- N2) sleep - '3' = REM sleep - '4' = awake
hypnogram :: Sleep -> [(ZonedTime, SleepPhase)]
hypnogram Sleep{_sleep_bedtime_start, _sleep_hypnogram_5min} =
  zip (windows _sleep_bedtime_start 300) (charToPhase <$> _sleep_hypnogram_5min)

instance FromJSON Sleep where
  parseJSON = genericParseJSON (withoutPrefix "_sleep_")

makeLenses ''Sleep

data OuraData = OuraData {
  _activity          :: Maybe [Activity]
  , _readiness       :: Maybe [Readiness]
  , _restful_periods :: Maybe [RestfulPeriod]
  , _sleep           :: Maybe [Sleep]
  } deriving (Generic, Show)

makeLenses ''OuraData

instance FromJSON OuraData where
  parseJSON = genericParseJSON (defaultOptions{fieldLabelModifier=dropWhile (== '_')})
