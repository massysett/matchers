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
import qualified Data.Prednote.Pdct as R

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

  -> Either Text (R.Pdct Text)
  -- ^ The Pdct if the pattern is good; if the pattern is bad,
  -- returns an error message.

pcre c t = case PCRE.compile (c == Insensitive) t of
  Left e -> Left . pack $ e
  Right r ->
    let mrDesc = pack $ "matches the PCRE pattern \""
          ++ unpack t ++ "\"" ++ descSensitive c
        mr = maybe False id . PCRE.exec r
    in return $ R.operand mrDesc mr

-- | Matcher that succeeds if the pattern text is found anywhere
-- within the subject.
within
  :: CaseSensitive

  -> Text
  -- ^ The pattern

  -> R.Pdct Text
within cs t = R.operand mrDesc mr
  where
    mrDesc = pack $ "contains the text \"" ++ unpack t
             ++ "\"" ++ descSensitive cs
    mr = txtMatch isInfixOf cs t

-- | Matcher that succeeds if the pattern text exactly matches the
-- subject (with case sensitivity as appropriate.)
exact :: CaseSensitive -> Text -> R.Pdct Text
exact cs t = R.operand mrDesc mr
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
anyTime :: R.Pdct Text
anyTime = R.operand mrDesc mr
  where
    mrDesc = pack "any valid time"
    mr x = case P.parse dateTime "" x of
      Left _ -> False
      Right _ -> True

-- | If the given ordering is @r@, the given time is @t@, and the
-- time of the subject is @s@, the Pdct returns @compare s t == r@.
-- Always returns False if the subject is not a valid time.
time
  :: Ordering
  -- ^ @r@
  -> Time.UTCTime
  -- ^ @t@
  -> R.Pdct Text
time ord ti = R.operand mrDesc mr
  where
    mrDesc = pack $ "subject time is " ++ d ++ " " ++ show ti
      where
        d = case ord of
          EQ -> "equal to"
          LT -> "less than"
          GT -> "greater than"
    mr x = case P.parse dateTime "" x of
      Left _ -> False
      Right g -> compare g ti == ord

