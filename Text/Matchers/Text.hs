module Text.Matchers.Text (
  tdfa, pcre, within, exact,
  CaseSensitive(Sensitive, Insensitive)) where

import Control.Monad.Exception.Synchronous
  ( Exceptional (Exception, Success))
import Data.Text (Text, pack, unpack, toCaseFold, isInfixOf)
import Data.Text.Encoding (encodeUtf8)

import qualified Text.Matchers.Regex.PCRE as PCRE
import qualified Text.Matchers.Regex.TDFA as TDFA
import Text.Matchers.CaseSensitive
  (CaseSensitive(Sensitive, Insensitive))

tdfa :: CaseSensitive
        -> Text
        -> Exceptional Text (Text -> Bool)
tdfa c t = case TDFA.tdfa c (unpack t) of
  (Exception e) -> Exception (pack e)
  (Success f) -> return (f . unpack)

pcre :: CaseSensitive
        -> Text
        -> Exceptional Text (Text -> Bool)
pcre c t = case PCRE.pcre c (encodeUtf8 t) of
  (Exception e) -> Exception (pack e)
  (Success f) -> return (f . encodeUtf8)

within :: CaseSensitive -> Text -> Text -> Bool
within = txtMatch isInfixOf

exact :: CaseSensitive -> Text -> Text -> Bool
exact = txtMatch (==)

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
