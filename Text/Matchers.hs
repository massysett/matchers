module Text.Matchers
  ( CaseSensitive(..)
  , pcre
  , within
  , exact
  , anyTime
  , time
  ) where

import Data.Text (Text, pack, unpack, toCaseFold, isInfixOf)
import qualified Text.Parsec as P
import qualified Data.Time as Time
import Text.Matchers.Times (dateTime)
import Text.Matchers.Pcre as PCRE
import qualified Data.Prednote.Predbox as R

data CaseSensitive = Sensitive | Insensitive deriving (Eq, Ord, Show)

descSensitive :: CaseSensitive -> String
descSensitive c = case c of
  Sensitive -> " (case sensitive)"
  Insensitive -> " (case insensitive)"

-- | Uses the PCRE regular expression engine.
pcre
  :: CaseSensitive

  -> Text
  -- ^ Pattern

  -> Either Text (R.Predbox Text)
  -- ^ The Predbox if the pattern is good; if the pattern is bad,
  -- returns an error message.

pcre c t = case PCRE.compile (c == Insensitive) t of
  Left e -> Left . pack $ e
  Right r ->
    let mrDesc = pack $ "matches the PCRE pattern \""
          ++ unpack t ++ "\"" ++ descSensitive c
        mr = maybe False id . PCRE.exec r
    in return $ R.predicate mrDesc mr

-- | Matcher that succeeds if the pattern text is found anywhere
-- within the subject.
within
  :: CaseSensitive

  -> Text
  -- ^ The pattern

  -> R.Predbox Text
within cs t = R.predicate mrDesc mr
  where
    mrDesc = pack $ "contains the text \"" ++ unpack t
             ++ "\"" ++ descSensitive cs
    mr = txtMatch isInfixOf cs t

-- | Matcher that succeeds if the pattern text exactly matches the
-- subject (with case sensitivity as appropriate.)
exact :: CaseSensitive -> Text -> R.Predbox Text
exact cs t = R.predicate mrDesc mr
  where
    mrDesc = pack $ "matches the text \"" ++ unpack t
             ++ "\"" ++ descSensitive cs
    mr = txtMatch (==) cs t

txtMatch :: (Text -> Text -> Bool)
            -> CaseSensitive
            -> Text
            -> Text -> Bool
txtMatch f c p t = pat `f` txt where
  txt = flipCase t
  pat = flipCase p
  flipCase = case c of
    Sensitive -> id
    Insensitive -> toCaseFold

-- | Matches any valid time.
anyTime :: R.Predbox Text
anyTime = R.predicate mrDesc mr
  where
    mrDesc = pack "any valid time"
    mr x = case P.parse dateTime "" x of
      Left _ -> False
      Right _ -> True

-- | If the given ordering is @r@, the given time is @t@, and the
-- time of the subject is @s@, the Predbox returns @compare s t == r@.
-- Always returns False if the subject is not a valid time.
time
  :: Ordering
  -- ^ @r@
  -> Time.UTCTime
  -- ^ @t@
  -> R.Predbox Text
time ord ti = R.compareByMaybe desc (pack "time") mr ord
  where
    desc = pack . show $ ti
    mr x = case P.parse dateTime "" x of
      Left _ -> Nothing
      Right g -> Just $ (g `compare` ti)

