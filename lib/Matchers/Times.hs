-- | Time parsers.
--
-- "Matchers" allows you to perform matching based on times.
-- Times are parsed using the parsers in this module.
module Matchers.Times where

import Data.Fixed
import Data.Maybe
import Control.Applicative
import Control.Monad
import qualified Data.Time as Time
import Text.Parsec (satisfy)
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

-- | A four-digit year.
year :: Parser Integer
year = read <$> replicateM 4 P.digit

-- | A two-digit month (exactly 2 digits.)
month :: Parser Int
month = read <$> replicateM 2 P.digit

-- | A two-digit day (exactly 2 digits.)
day :: Parser Int
day = read <$> replicateM 2 P.digit

-- | A valid Gregorian day, in YYYY-MM-DD format.  Each separator
-- may be a hyphen or a slash.  Fails if the day is not valid.
pDate :: Parser Time.Day
pDate = p >>= failOnErr
  where
    p = Time.fromGregorianValid
        <$> year  <* satisfy dateSep
        <*> month <* satisfy dateSep
        <*> day
    failOnErr = maybe (fail "could not parse date") return

-- | Date separator (slash or hyphen).
dateSep :: Char -> Bool
dateSep c = c == '/' || c == '-'

digit :: Char -> Bool
digit c = c >= '0' && c <= '9'

colon :: Char -> Bool
colon = (== ':')

-- | Two digits for the hour (exactly two digits).  Must be between
-- 0 and 23.
hours :: Parser Int
hours = p >>= (maybe (fail "could not parse hours") return)
  where
    p = f <$> satisfy digit <*> satisfy digit
    f d1 d2 =
      let r = read [d1,d2]
      in if r < 0 || r > 23
         then Nothing
         else Just r


-- | Two digits for the minutes (exactly two digits).  Must be
-- between 0 and 59.
minutes :: Parser Int
minutes = p >>= maybe (fail "could not parse minutes") return
  where
    p = f <$ satisfy colon <*> satisfy digit <*> satisfy digit
    f d1 d2 =
      let r = read [d1, d2]
      in if r < 0 || r > 59
         then Nothing
         else Just r

-- | Two digits for seconds (exactly two digits).  Must be between 0
-- and 59; there are no leap seconds.
seconds :: Parser Pico
seconds = p >>= maybe (fail "could not parse seconds") return
  where
    p = f <$ satisfy colon <*> satisfy digit <*> satisfy digit
    f d1 d2 =
      let r = read [d1, d2] :: Int
      in if r < 0 || r > 59
         then Nothing
         else Just . fromIntegral $ r

-- | Hours and minutes, separated by colons, with optional seconds.
time :: Parser Time.TimeOfDay
time = f <$> hours <*> minutes <*> optional seconds
  where
    f h m ms = Time.TimeOfDay h m (fromMaybe 0 ms)

-- | Time zone sign, plus or minus.
tzSign :: Parser (Int -> Int)
tzSign = (id <$ satisfy plus) <|> (negate <$ satisfy minus)
  where
    plus = (== '+')
    minus = (== '-')

-- | Time zone offset, exactly 4 digits.
tzNumber :: Parser Int
tzNumber = read <$> replicateM 4 (satisfy digit)

-- | Time zone; that is, sign and offset.  Both the sign and offset
-- are required.  The number of minutes may not exceed 840.
timeZone :: Parser Time.TimeZone
timeZone = p >>= maybe (fail "could not parse time zone") return
  where
    p = f <$> tzSign <*> tzNumber
    f s = minsToOffset . s
    minsToOffset m = if abs m > 840
                     then Nothing
                     else Just (Time.TimeZone m False "")

-- | Space or tab.
white :: Char -> Bool
white c = c == ' ' || c == '\t'

-- | Time of day, with optional time zone.
timeWithZone :: Parser (Time.TimeOfDay, Maybe Time.TimeZone)
timeWithZone =
  (,) <$> time <* many (satisfy white) <*> optional timeZone


-- | Day, followed by optional whitespace, followed by optional time
-- with zone.
dateTime :: Parser Time.UTCTime
dateTime =
  f <$> pDate <* many (satisfy white) <*> optional timeWithZone
  where
    f d mayTwithZ = Time.zonedTimeToUTC zt
      where
        zt = Time.ZonedTime lt tz
        lt = Time.LocalTime d tod
        (tod, tz) = case mayTwithZ of
          Nothing -> (Time.midnight, Time.utc)
          Just (t, mayZ) -> case mayZ of
            Nothing -> (t, Time.utc)
            Just z -> (t, z)

