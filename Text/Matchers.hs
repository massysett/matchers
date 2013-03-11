module Text.Matchers
  ( Matcher(..)
  , CaseSensitive(..)
  , tdfa
  , pcre
  , within
  , exact
  , CompUTC(..)
  , descUTC
  , compUTCtoCmp
  , date
  ) where

import Control.Applicative ((<$>), (<*>), (<*), (<$), optional, (<|>))
import Control.Monad (replicateM, mzero)
import Control.Monad.Exception.Synchronous
  ( Exceptional (Exception, Success))
import qualified Data.ByteString as BS
import Data.Fixed (Pico)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack, toCaseFold, isInfixOf)
import Data.Text.Encoding (encodeUtf8)
import qualified Text.Regex.TDFA as TDFA
import qualified Text.Regex.Base.RegexLike as RL
import qualified Text.Regex.PCRE.Light as PCRE
import Text.Parsec (many, satisfy)
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import qualified Data.Time as Time

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

-- | Uses the regular expression matcher from the regex-tdfa
-- package. This is a POSIX extended regular expression. It should
-- work correctly with Unicode.
tdfa
  :: CaseSensitive

  -> Text
  -- ^ The pattern

  -> Exceptional Text Matcher
  -- ^ The Matcher if the pattern is good; if the pattern is bad,
  -- returns an error message. The error message has a trailing
  -- newline.

tdfa c t = case tdfaPrim c (unpack t) of
  Exception e -> Exception $ pack e
  Success f ->
    let sDesc = pack "POSIX-like regular expression (TDFA)"
        mrDesc = pack $ "matches the POSIX regular expression \""
          ++ unpack t ++ "\"" ++ descSensitive c
        mr = f . unpack
    in return $ Matcher sDesc mrDesc mr

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

  -> Exceptional Text Matcher
  -- ^ The Matcher if the pattern is good; if the pattern is bad,
  -- returns an error message.

pcre c t = case pcrePrim c (encodeUtf8 t) of
  Exception e -> Exception $ pack e
  Success f ->
    let sDesc = pack "Perl-compatible regular expression"
        mrDesc = pack $ "matches the PCRE pattern \""
          ++ unpack t ++ "\"" ++ descSensitive c
        mr = f . encodeUtf8
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
-- StrErr monad
------------------------------------------------------------

-- | This monad exists because using the mtl Monad instance of (Either
-- String) causes problems due to orphan instances.
--
-- See http://www.haskell.org/pipermail/haskell-cafe/2011-December/098079.html

data StrErr a = Good a
              | Bad String
              deriving (Show, Eq)

instance Monad StrErr where
  return = Good
  (Good a) >>= f = f a
  (Bad s) >>= _ = Bad s
  fail s = Bad s

------------------------------------------------------------
-- TDFA primitives
------------------------------------------------------------

tdfaPrim
  :: CaseSensitive -> String -> Exceptional String (String -> Bool)
tdfaPrim c regexStr = case RL.makeRegexOptsM comp exec regexStr of
  (Bad s) -> Exception s
  (Good rx) -> return (RL.matchTest rx)
  where
    comp = RL.defaultCompOpt { TDFA.caseSensitive = case c of
                                  Sensitive -> True
                                  Insensitive -> False
                             , TDFA.newSyntax = True
                             , TDFA.lastStarGreedy = True }
    exec = RL.defaultExecOpt { TDFA.captureGroups = False }

------------------------------------------------------------
-- PCRE primitives
------------------------------------------------------------

pcrePrim :: CaseSensitive
        -> BS.ByteString
        -> Exceptional String (BS.ByteString -> Bool)
pcrePrim c bs = let
  u8 = [PCRE.utf8]
  opts = case c of
    Sensitive -> u8
    Insensitive -> PCRE.caseless:u8 in
  case PCRE.compileM bs opts of
    (Left err) -> Exception err
    (Right rx) -> Success $ \s ->
      case PCRE.match rx s [] of
        (Just _) -> True
        Nothing -> False

------------------------------------------------------------
-- Date parsers
------------------------------------------------------------
year :: Parser Integer
year = read <$> replicateM 4 P.digit

month :: Parser Int
month = read <$> replicateM 2 P.digit

day :: Parser Int
day = read <$> replicateM 2 P.digit

pDate :: Parser Time.Day
pDate = p >>= failOnErr
  where
    p = Time.fromGregorianValid
        <$> year  <* satisfy dateSep
        <*> month <* satisfy dateSep
        <*> day
    failOnErr = maybe (fail "could not parse date") return

dateSep :: Char -> Bool
dateSep c = c == '/' || c == '-'

digit :: Char -> Bool
digit c = c >= '0' && c <= '9'

colon :: Char -> Bool
colon = (== ':')

hours :: Parser Int
hours = p >>= (maybe (fail "could not parse hours") return)
  where
    p = f <$> satisfy digit <*> satisfy digit
    f d1 d2 =
      let r = read [d1,d2]
      in if r < 0 || r > 23
         then Nothing
         else Just r


minutes :: Parser Int
minutes = p >>= maybe (fail "could not parse minutes") return
  where
    p = f <$ satisfy colon <*> satisfy digit <*> satisfy digit
    f d1 d2 =
      let r = read [d1, d2]
      in if r < 0 || r > 59
         then Nothing
         else Just r

seconds :: Parser Pico
seconds = p >>= maybe (fail "could not parse seconds") return
  where
    p = f <$ satisfy colon <*> satisfy digit <*> satisfy digit
    f d1 d2 =
      let r = read [d1, d2] :: Int
      in if r < 0 || r > 59
         then Nothing
         else Just . fromIntegral $ r

time :: Parser Time.TimeOfDay
time = f <$> hours <*> minutes <*> optional seconds
  where
    f h m ms = Time.TimeOfDay h m (fromMaybe 0 ms)

tzSign :: Parser (Int -> Int)
tzSign = (id <$ satisfy plus) <|> (negate <$ satisfy minus)
  where
    plus = (== '+')
    minus = (== '-')

tzNumber :: Parser Int
tzNumber = read <$> replicateM 4 (satisfy digit)

timeZone :: Parser Time.TimeZone
timeZone = p >>= maybe (fail "could not parse time zone") return
  where
    p = f <$> tzSign <*> tzNumber
    f s = minsToOffset . s
    minsToOffset m = if abs m > 840
                     then Nothing
                     else Just (Time.TimeZone m False "")

white :: Char -> Bool
white c = c == ' ' || c == '\t'

timeWithZone :: Parser (Time.TimeOfDay, Maybe Time.TimeZone)
timeWithZone =
  (,) <$> time <* many (satisfy white) <*> optional timeZone


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

