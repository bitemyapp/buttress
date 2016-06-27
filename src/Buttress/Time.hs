{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Buttress.Time where

import Data.Function
import Data.Time.Clock

-- 45.minutes + 2.hours + 4.years.
-- # equivalent to Time.current.advance(months: 1)
-- 1.month.from_now
 
-- # equivalent to Time.current.advance(years: 2)
-- 2.years.from_now
 
-- # equivalent to Time.current.advance(months: 4, years: 5)
-- (4.months + 5.years).from_now

-- def days_in_year(year = current.year)
--   days_in_month(2, year) + 337
-- end

-- newtype TimeConvert =
--   TimeConvert DiffTime
--   deriving (Eq, Show)

-- instance Num TimeConvert where
--   (+) (TimeConvert dt) (TimeConvert dt') =
--     TimeConvert $ dt + dt'
--   (-) (TimeConvert dt) (TimeConvert dt') =
--     TimeConvert $ dt - dt'
--   (*) (TimeConvert dt) (TimeConvert dt') =
--     TimeConvert $ dt * dt'
--   abs (TimeConvert dt) =
--     TimeConvert $ abs dt
--   signum (TimeConvert dt) =
--     TimeConvert $ signum dt
--   fromInteger (TimeConvert dt) (TimeConvert dt') =
--     TimeConvert $ dt * dt'

data TimeConvert =
    Seconds
  | Hours
  | Days
  | Weeks
  | Months
  | Years
  deriving (Eq, Show)

years = Years
year = years

instance (b ~ DiffTime) => Num (TimeConvert -> b) where
  fromInteger i Years = secondsToDiffTime (i * secondsInAYear)

instance (b ~ DiffTime) => Fractional (TimeConvert -> b) where
  fromRational r Years = secondsToDiffTime (round (r * (toRational secondsInAYear)))

-- instance Num (TimeConvert -> DiffTime) where
--   fromInteger i Years = years i

-- *Buttress.Time> 1 Years :: DiffTime
-- 31557600s

-- addition and subtraction are fine, but multiplication isn't really kosher.
-- newtype TimeSpan =
--   TimeSpan { unTimeSpan :: DiffTime }
--   deriving (Eq)

-- instance Show TimeSpan where
--   show (TimeSpan dt) = undefined

secondsInADay :: Integer
secondsInADay = 86400

secondsInAYear :: Integer
secondsInAYear = (86400 * 365) + (div 86400 4)

-- day, days :: Integer -> DiffTime
-- days a = secondsToDiffTime (secondsInADay * a)
-- day = days

-- year, years :: Integer -> DiffTime
-- years a = secondsToDiffTime (secondsInAYear * a)
-- year = years
