module Text.Matchers.Regex.PCRE (pcre) where

import Control.Monad.Exception.Synchronous
  ( Exceptional (Exception, Success))
import qualified Data.ByteString as BS
import qualified Text.Regex.PCRE.Light as PCRE

import Text.Matchers.CaseSensitive
  (CaseSensitive(Sensitive, Insensitive))

