module Text.Matchers.String (
  tdfa, pcre, within, exact,
  CaseSensitive(Sensitive, Insensitive)) where

import Control.Monad.Exception.Synchronous
  ( Exceptional (Exception, Success))
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.String.UTF8 (fromString, toRep)

import qualified Text.Matchers.Regex.TDFA as TDFA
import qualified Text.Matchers.Regex.PCRE as PCRE
import Text.Matchers.CaseSensitive
  (CaseSensitive(Sensitive, Insensitive))

tdfa :: CaseSensitive
        -> String
        -> Exceptional String (String -> Bool)
tdfa = TDFA.tdfa

pcre :: CaseSensitive
        -> String
        -> Exceptional String (String -> Bool)
pcre c s = case PCRE.pcre c (toRep . fromString $ s) of
  (Success f) -> return (f . toRep . fromString)
  (Exception e) -> Exception e

within :: CaseSensitive -> String -> String -> Bool
within = strMatch isInfixOf

exact :: CaseSensitive -> String -> String -> Bool
exact = strMatch (==)

strMatch :: (String -> String -> Bool)
            -> CaseSensitive
            -> String
            -> String -> Bool
strMatch f c s t = pat `f` str where
  str = flipCase t
  pat = flipCase s
  flipCase = case c of
    Sensitive -> id
    Insensitive -> map toLower
