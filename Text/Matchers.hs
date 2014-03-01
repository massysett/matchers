module Text.Matchers
  ( Matcher(..)
  , CaseSensitive(..)
  , pcre
  , within
  , exact
  , CompUTC(..)
  , descUTC
  , compUTCtoCmp
  , date
  ) where

import Control.Monad (mzero)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack, toCaseFold, isInfixOf)
import qualified Text.Parsec as P
import qualified Data.Time as Time
import Text.Matchers.Dates
import Text.Matchers.Pcre as PCRE

data CaseSensitive = Sensitive | Insensitive deriving (Eq, Ord, Show)

data Matcher = Matcher
  { shortDesc :: Text
    -- ^ Short description of this matcher, e.g. @PCRE@ or @Exact@.

  , matchDesc :: Text
    -- ^ Description of a successful match, e.g.
    -- @Matches the PCRE pattern abc@, or
    -- @a valid date with optional time@.

  , match :: Text -> Bool
    -- ^ Function to carry out the match
    }

descSensitive :: CaseSensitive -> String
descSensitive c = case c of
  Sensitive -> " (case sensitive)"
  Insensitive -> " (case insensitive)"

-- | Uses the PCRE regular expression engine. Currently the pcre-light
-- package is used, as it has a simpler interface than the
-- regex-pcre-builtin. It should work correctly with Unicode.
pcre
  :: CaseSensitive

  -> Text
  -- ^ Pattern

  -> Either Text Matcher
  -- ^ The Matcher if the pattern is good; if the pattern is bad,
  -- returns an error message.

pcre c t = case PCRE.compile (c == Insensitive) t of
  Left e -> Left . pack $ e
  Right r ->
    let sDesc = pack "Perl-compatible regular expression"
        mrDesc = pack $ "matches the PCRE pattern \""
          ++ unpack t ++ "\"" ++ descSensitive c
        mr = maybe False id . PCRE.exec r
    in return $ Matcher sDesc mrDesc mr

-- | Matcher that succeeds if the pattern text is found anywhere
-- within the subject.
within
  :: CaseSensitive

  -> Text
  -- ^ The pattern

  -> Matcher
within cs t = Matcher sDesc mrDesc mr
  where
    sDesc = pack "within"
    mrDesc = pack $ "contains the text \"" ++ unpack t
             ++ "\"" ++ descSensitive cs
    mr = txtMatch isInfixOf cs t

-- | Matcher that succeeds if the pattern text exactly matches the
-- subject (with case sensitivity as appropriate.)
exact :: CaseSensitive -> Text -> Matcher
exact cs t = Matcher sDesc mrDesc mr
  where
    sDesc = pack "exact"
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

-- | Matcher that succeeds if the subject represents a valid date with
-- an optional time.
date
  :: Maybe (CompUTC, Time.UTCTime)
  -- ^ If Nothing, any valid date and time will succeed as a match;
  -- the matcher will return False if the subject is not a valid
  -- date. If Just, the subject must be a valid date and must fit
  -- within the range indicated.
  -> Matcher
date mayPair = Matcher (pack "date") md mr
  where
    md = case mayPair of
      Nothing -> pack "any valid date with optional time"
      Just (c, t) -> pack $ "valid date and optional time, "
        ++ descUTC c t
    mr x = fromMaybe False $ do
      subjDT <- case P.parse dateTime "" x of
        Left _ -> mzero
        Right g -> return g
      case mayPair of
        Nothing -> return True
        Just (c, t) ->
          let cmp = compUTCtoCmp c
          in return $ subjDT `cmp` t

------------------------------------------------------------
-- Other date things
------------------------------------------------------------

data CompUTC
  = UAfter
  | UOnOrAfter
  | UExactly
  | UBefore
  | UOnOrBefore
  deriving (Eq, Show, Ord)

descUTC :: CompUTC -> Time.UTCTime -> String
descUTC c u = "date is " ++ co ++ " " ++ dt
  where
    co = case c of
      UAfter -> "after"
      UOnOrAfter -> "on or after"
      UExactly -> "on"
      UBefore -> "before"
      UOnOrBefore -> "on or before"
    dt = show dy ++ " " ++ hs ++ ":" ++ ms ++ ":" ++ ss ++ " UTC"
    Time.UTCTime dy difft = u
    Time.TimeOfDay h m s = Time.timeToTimeOfDay difft
    (hs, ms, ss) = (show h, show m, show (round s :: Int))

compUTCtoCmp :: Ord a => CompUTC -> a -> a -> Bool
compUTCtoCmp c = case c of
  UAfter -> (>)
  UOnOrAfter -> (>=)
  UExactly -> (==)
  UBefore -> (<)
  UOnOrBefore -> (<=)

