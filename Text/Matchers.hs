module Text.Matchers
  ( Matcher(..)
  , CaseSensitive(..)
  , tdfa
  , pcre
  , within
  , exact
  ) where

import Control.Monad.Exception.Synchronous
  ( Exceptional (Exception, Success))
import qualified Data.ByteString as BS
import Data.Text (Text, pack, unpack, toCaseFold, isInfixOf)
import Data.Text.Encoding (encodeUtf8)
import qualified Text.Regex.TDFA as TDFA
import qualified Text.Regex.Base.RegexLike as RL
import qualified Text.Regex.PCRE.Light as PCRE

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

tdfa :: CaseSensitive -> Text -> Exceptional Text Matcher
tdfa c t = case tdfaPrim c (unpack t) of
  Exception e -> Exception (pack e)
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

pcre :: CaseSensitive -> Text -> Exceptional Text Matcher
pcre c t = case pcrePrim c (encodeUtf8 t) of
  Exception e -> Exception (pack e)
  Success f ->
    let sDesc = pack "Perl-compatible regular expression"
        mrDesc = pack $ "matches the PCRE pattern \""
          ++ unpack t ++ "\"" ++ descSensitive c
        mr = f . encodeUtf8
    in return $ Matcher sDesc mrDesc mr

within :: CaseSensitive -> Text -> Matcher
within cs t = Matcher sDesc mrDesc mr
  where
    sDesc = pack "within"
    mrDesc = pack $ "contains the text \"" ++ unpack t
             ++ "\"" ++ descSensitive cs
    mr = txtMatch isInfixOf cs t

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

